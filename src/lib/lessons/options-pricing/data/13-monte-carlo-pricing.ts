import type { Lesson } from "../../types";

export const monteCarloPricing: Lesson = {
  id: "monte-carlo-pricing",
  title: "Monte Carlo Pricing",
  chapterId: "numerical-methods",
  content: `## Monte Carlo Pricing

**Monte Carlo simulation** is the most flexible method for pricing options. Instead of solving equations analytically, we simulate thousands of possible stock price paths and average the payoffs.

### The Stock Price Model

Under risk-neutral measure, the stock price follows:

$$S_T = S \\cdot \\exp\\left((r - \\frac{\\sigma^2}{2})T + \\sigma\\sqrt{T} \\cdot Z\\right)$$

where Z ~ N(0, 1) is a standard normal random variable.

### Monte Carlo Algorithm

1. For each of n_paths simulations:
   - Sample Z from N(0,1)
   - Compute $S_T$ using the formula above
   - Calculate the payoff: max(S_T - K, 0)
2. Average all payoffs
3. Discount at risk-free rate: $C \\approx e^{-rT} \\cdot \\text{mean payoff}$

### Generating Normal Samples: Box-Muller

To generate standard normal samples from uniform random numbers:

$$Z_1 = \\sqrt{-2\\ln U_1} \\cos(2\\pi U_2)$$
$$Z_2 = \\sqrt{-2\\ln U_1} \\sin(2\\pi U_2)$$

where U₁, U₂ are independent Uniform(0,1).

### Accuracy

With n_paths = 10,000, the standard error is roughly σ/√n ≈ 1% of the price. More paths = more accuracy but slower computation.`,
  starterCode: `import math
import random

def box_muller(u1, u2):
    """Generate two standard normal samples from two uniform samples."""
    z1 = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
    z2 = math.sqrt(-2 * math.log(u1)) * math.sin(2 * math.pi * u2)
    return z1, z2

def mc_call_price(S, K, T, r, sigma, n_paths, seed):
    """Price a European call using Monte Carlo simulation."""
    pass`,
  solution: `import math
import random

def box_muller(u1, u2):
    """Generate two standard normal samples from two uniform samples."""
    z1 = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
    z2 = math.sqrt(-2 * math.log(u1)) * math.sin(2 * math.pi * u2)
    return z1, z2

def mc_call_price(S, K, T, r, sigma, n_paths, seed):
    """Price a European call using Monte Carlo simulation."""
    rng = random.Random(seed)
    total = 0.0
    i = 0
    while i < n_paths:
        u1 = rng.random()
        u2 = rng.random()
        z1, z2 = box_muller(u1, u2)
        for z in [z1, z2]:
            if i >= n_paths:
                break
            ST = S * math.exp((r - sigma**2 / 2) * T + sigma * math.sqrt(T) * z)
            total += max(ST - K, 0)
            i += 1
    return math.exp(-r * T) * total / n_paths`,
  tests: [
    {
      name: "MC call price (S=K=100, T=1, r=0.05, sigma=0.2, n=10000, seed=42)",
      code: `{{FUNC}}\nprint(round(mc_call_price(100, 100, 1, 0.05, 0.2, 10000, 42), 4))`,
      expected: "10.4269\n",
    },
    {
      name: "MC call price with more paths (closer to BS)",
      code: `{{FUNC}}\nprint(round(mc_call_price(100, 100, 1, 0.05, 0.2, 50000, 42), 4))`,
      expected: "10.4425\n",
    },
    {
      name: "MC call ITM (S=110, K=100, T=1, r=0.05, sigma=0.25, n=10000, seed=42)",
      code: `{{FUNC}}\nprint(round(mc_call_price(110, 100, 1, 0.05, 0.25, 10000, 42), 4))`,
      expected: "19.2489\n",
    },
    {
      name: "MC price within 5% of BS price",
      code: `{{FUNC}}\nimport math\ndef norm_cdf(x):\n    return 0.5 * math.erfc(-x / math.sqrt(2))\ndef bs_call(S, K, T, r, sigma):\n    d1 = (math.log(S/K) + (r + sigma**2/2)*T) / (sigma*math.sqrt(T))\n    d2 = d1 - sigma*math.sqrt(T)\n    return S*norm_cdf(d1) - K*math.exp(-r*T)*norm_cdf(d2)\nmc = mc_call_price(100, 100, 1, 0.05, 0.2, 10000, 42)\nbs = bs_call(100, 100, 1, 0.05, 0.2)\nprint(abs(mc - bs) / bs < 0.05)`,
      expected: "True\n",
    },
  ],
};
