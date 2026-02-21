import type { Lesson } from "../../types";

export const bsGamma: Lesson = {
  id: "bs-gamma",
  title: "Gamma",
  chapterId: "greeks",
  content: `## Gamma: Rate of Change of Delta

**Gamma** (Γ) measures how much delta changes for a $1 move in the underlying:

$$\\Gamma = \\frac{\\partial \\Delta}{\\partial S} = \\frac{\\partial^2 V}{\\partial S^2}$$

### Black-Scholes Gamma

Gamma is the same for both calls and puts (put-call parity):

$$\\Gamma = \\frac{N'(d_1)}{S \\cdot \\sigma \\cdot \\sqrt{T}}$$

where $N'(x)$ is the standard normal PDF:

$$N'(x) = \\frac{e^{-x^2/2}}{\\sqrt{2\\pi}}$$

### Interpretation

- Gamma measures the **curvature** of the option price with respect to the stock price
- High gamma means delta changes rapidly — the hedge needs frequent rebalancing
- Gamma is highest for **ATM options** near expiry
- Gamma is always **positive** for long options (calls and puts)

### Why Gamma Matters

If you hold a delta-neutral portfolio and the stock makes a large move, gamma tells you how much your delta has drifted. A high-gamma position benefits more from large moves (volatility) but requires more frequent rebalancing.

### Relationship to Theta

Long gamma (long options) tends to come with negative theta — you pay for the convexity through time decay.`,
  starterCode: `import math

def norm_pdf(x):
    """Standard normal PDF."""
    return math.exp(-x**2 / 2) / math.sqrt(2 * math.pi)

def bs_gamma(S, K, T, r, sigma):
    """Gamma of a European option (same for calls and puts)."""
    pass`,
  solution: `import math

def norm_pdf(x):
    """Standard normal PDF."""
    return math.exp(-x**2 / 2) / math.sqrt(2 * math.pi)

def bs_gamma(S, K, T, r, sigma):
    """Gamma of a European option (same for calls and puts)."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    return norm_pdf(d1) / (S * sigma * math.sqrt(T))`,
  tests: [
    {
      name: "gamma ATM (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_gamma(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "0.0188\n",
    },
    {
      name: "gamma ITM (S=110, K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_gamma(110, 100, 1, 0.05, 0.2), 4))`,
      expected: "0.0129\n",
    },
    {
      name: "gamma ATM shorter expiry (T=0.5)",
      code: `{{FUNC}}\nprint(round(bs_gamma(100, 100, 0.5, 0.05, 0.2), 4))`,
      expected: "0.0274\n",
    },
    {
      name: "gamma is always positive",
      code: `{{FUNC}}\nprint(bs_gamma(100, 100, 1, 0.05, 0.2) > 0)`,
      expected: "True\n",
    },
  ],
};
