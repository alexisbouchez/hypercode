import type { Lesson } from "../../types";

export const bsTheta: Lesson = {
  id: "bs-theta",
  title: "Theta (Time Decay)",
  chapterId: "greeks",
  content: `## Theta: Time Decay

**Theta** (Θ) measures how much the option price decreases as time passes (holding everything else constant):

$$\\Theta = \\frac{\\partial V}{\\partial t}$$

In practice, theta is usually expressed as the **daily** loss in option value.

### Black-Scholes Theta for a Call

The annual theta for a European call is:

$$\\Theta_{\\text{annual}} = -\\frac{S \\cdot N'(d_1) \\cdot \\sigma}{2\\sqrt{T}} - r \\cdot K \\cdot e^{-rT} \\cdot N(d_2)$$

The **daily theta** divides by 365:

$$\\Theta_{\\text{daily}} = \\frac{\\Theta_{\\text{annual}}}{365}$$

### Why is Theta Negative?

Options lose value as expiry approaches (for long positions). With less time remaining, there's less opportunity for the stock to move in a favorable direction. This is called **time decay** or **theta decay**.

### Key Properties

- Theta is **most negative** for ATM options near expiry
- Deep ITM and deep OTM options have less time value to lose
- **Sellers** of options profit from theta (positive theta)
- **Buyers** of options suffer from theta (negative theta)

### The Gamma-Theta Trade-off

Long gamma (benefits from large moves) comes with short theta (hurt by time passing). This is the core trade-off in options trading.`,
  starterCode: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def norm_pdf(x):
    return math.exp(-x**2 / 2) / math.sqrt(2 * math.pi)

def bs_theta_call(S, K, T, r, sigma):
    """Daily theta of a European call option."""
    pass`,
  solution: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def norm_pdf(x):
    return math.exp(-x**2 / 2) / math.sqrt(2 * math.pi)

def bs_theta_call(S, K, T, r, sigma):
    """Daily theta of a European call option."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    annual = -S * norm_pdf(d1) * sigma / (2 * math.sqrt(T)) - r * K * math.exp(-r * T) * norm_cdf(d2)
    return annual / 365`,
  tests: [
    {
      name: "theta ATM call daily (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_theta_call(100, 100, 1, 0.05, 0.2), 6))`,
      expected: "-0.017573\n",
    },
    {
      name: "theta is negative (options lose value over time)",
      code: `{{FUNC}}\nprint(bs_theta_call(100, 100, 1, 0.05, 0.2) < 0)`,
      expected: "True\n",
    },
    {
      name: "theta ITM call (S=110, K=100, T=1, r=0.05, sigma=0.25)",
      code: `{{FUNC}}\nprint(round(bs_theta_call(110, 100, 1, 0.05, 0.25), 6))`,
      expected: "-0.020519\n",
    },
    {
      name: "theta larger with shorter expiry (more time decay near expiry)",
      code: `{{FUNC}}\ntheta_1yr = bs_theta_call(100, 100, 1, 0.05, 0.2)\ntheta_half = bs_theta_call(100, 100, 0.5, 0.05, 0.2)\nprint(abs(theta_half) > abs(theta_1yr))`,
      expected: "True\n",
    },
  ],
};
