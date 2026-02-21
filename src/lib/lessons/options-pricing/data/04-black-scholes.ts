import type { Lesson } from "../../types";

export const blackScholes: Lesson = {
  id: "black-scholes",
  title: "Black-Scholes Formula",
  chapterId: "black-scholes",
  content: `## Black-Scholes Formula

The **Black-Scholes model** (1973) gives a closed-form price for European options under the assumption that the stock follows geometric Brownian motion.

### Inputs

| Symbol | Meaning |
|--------|---------|
| S | Current stock price |
| K | Strike price |
| T | Time to expiry (years) |
| r | Risk-free rate (continuously compounded) |
| σ | Volatility (annualized standard deviation of log-returns) |

### Formula

First compute $d_1$ and $d_2$:

$$d_1 = \\frac{\\ln(S/K) + (r + \\sigma^2/2)T}{\\sigma\\sqrt{T}}$$

$$d_2 = d_1 - \\sigma\\sqrt{T}$$

Then the call and put prices are:

$$C = S \\cdot N(d_1) - K e^{-rT} \\cdot N(d_2)$$

$$P = K e^{-rT} \\cdot N(-d_2) - S \\cdot N(-d_1)$$

where $N(x)$ is the standard normal CDF.

### Normal CDF via erfc

Python's \`math.erfc\` gives the complementary error function. We can compute:

$$N(x) = \\frac{1}{2} \\operatorname{erfc}\\!\\left(\\frac{-x}{\\sqrt{2}}\\right)$$

### Example Verification

For S=K=100, T=1, r=0.05, σ=0.2:
- d₁ = (ln(1) + 0.07) / 0.2 = 0.35
- d₂ = 0.35 - 0.2 = 0.15
- C ≈ 10.4506`,
  starterCode: `import math

def norm_cdf(x):
    """Standard normal CDF using math.erfc."""
    return 0.5 * math.erfc(-x / math.sqrt(2))

def bs_call(S, K, T, r, sigma):
    """Black-Scholes European call price."""
    pass

def bs_put(S, K, T, r, sigma):
    """Black-Scholes European put price."""
    pass`,
  solution: `import math

def norm_cdf(x):
    """Standard normal CDF using math.erfc."""
    return 0.5 * math.erfc(-x / math.sqrt(2))

def bs_call(S, K, T, r, sigma):
    """Black-Scholes European call price."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    return S * norm_cdf(d1) - K * math.exp(-r * T) * norm_cdf(d2)

def bs_put(S, K, T, r, sigma):
    """Black-Scholes European put price."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    return K * math.exp(-r * T) * norm_cdf(-d2) - S * norm_cdf(-d1)`,
  tests: [
    {
      name: "ATM call price (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_call(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "10.4506\n",
    },
    {
      name: "ATM put price (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_put(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "5.5735\n",
    },
    {
      name: "ITM call (S=110, K=100, T=1, r=0.05, sigma=0.25)",
      code: `{{FUNC}}\nprint(round(bs_call(110, 100, 1, 0.05, 0.25), 4))`,
      expected: "19.3051\n",
    },
    {
      name: "OTM put (S=90, K=100, T=0.5, r=0.03, sigma=0.3)",
      code: `{{FUNC}}\nprint(round(bs_put(90, 100, 0.5, 0.03, 0.3), 4))`,
      expected: "12.9257\n",
    },
  ],
};
