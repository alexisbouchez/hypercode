import type { Lesson } from "../../types";

export const binomial1step: Lesson = {
  id: "binomial-1step",
  title: "Binomial Tree (1-Step)",
  chapterId: "numerical-methods",
  content: `## Binomial Tree: 1-Step Model

The **binomial tree model** is a discrete-time approach to options pricing that doesn't require advanced mathematics — just basic probability and discounting.

### The 1-Step Setup

Over one time period T, the stock price can either:
- Go **up** by factor u: $S_u = S \\cdot u$
- Go **down** by factor d: $S_d = S \\cdot d$

### CRR Parameterization

The Cox-Ross-Rubinstein (CRR) parameterization ties the tree to Black-Scholes volatility:

$$u = e^{\\sigma\\sqrt{T}}, \\quad d = \\frac{1}{u} = e^{-\\sigma\\sqrt{T}}$$

### Risk-Neutral Probability

Under the risk-neutral measure, the probability of an up-move is:

$$p = \\frac{e^{rT} - d}{u - d}$$

This ensures the expected return equals the risk-free rate.

### Pricing the Option

1. Compute terminal payoffs: $C_u = \\max(S_u - K, 0)$ and $C_d = \\max(S_d - K, 0)$
2. Discount the expected payoff: $C = e^{-rT}(p \\cdot C_u + (1-p) \\cdot C_d)$

### Why Use Binomial?

- Intuitive and easy to implement
- Easily extended to American options (allow early exercise at each node)
- Converges to Black-Scholes as the number of steps increases`,
  starterCode: `import math

def binomial_call_1step(S, K, T, r, sigma):
    """Price a European call using a 1-step CRR binomial tree."""
    pass`,
  solution: `import math

def binomial_call_1step(S, K, T, r, sigma):
    """Price a European call using a 1-step CRR binomial tree."""
    u = math.exp(sigma * math.sqrt(T))
    d = 1 / u
    p = (math.exp(r * T) - d) / (u - d)
    Su = S * u
    Sd = S * d
    Cu = max(Su - K, 0)
    Cd = max(Sd - K, 0)
    return math.exp(-r * T) * (p * Cu + (1 - p) * Cd)`,
  tests: [
    {
      name: "1-step call ATM (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(binomial_call_1step(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "12.1623\n",
    },
    {
      name: "1-step call ITM (S=110, K=100, T=1, r=0.05, sigma=0.25)",
      code: `{{FUNC}}\nprint(round(binomial_call_1step(110, 100, 1, 0.05, 0.25), 4))`,
      expected: "21.1577\n",
    },
    {
      name: "1-step call shorter expiry (T=0.5)",
      code: `{{FUNC}}\nprint(round(binomial_call_1step(100, 100, 0.5, 0.05, 0.2), 4))`,
      expected: "8.2067\n",
    },
    {
      name: "price is non-negative",
      code: `{{FUNC}}\nprint(binomial_call_1step(100, 100, 1, 0.05, 0.2) > 0)`,
      expected: "True\n",
    },
  ],
};
