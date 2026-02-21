import type { Lesson } from "../../types";

export const americanOptions: Lesson = {
  id: "american-options",
  title: "American Options (Early Exercise)",
  chapterId: "numerical-methods",
  content: `## American Options and Early Exercise

**American options** can be exercised at any time up to expiry, unlike European options which can only be exercised at expiry.

### Why Early Exercise Matters

For American **puts**, early exercise can be optimal when the option is deep in-the-money. Exercising early allows you to:
- Receive the intrinsic value immediately
- Invest the proceeds at the risk-free rate

For American **calls** on non-dividend-paying stocks, early exercise is **never** optimal (you'd throw away the time value).

### Pricing with the Binomial Tree

The binomial tree handles American options naturally. At each node, compare:

1. **Continuation value**: the discounted expected value of holding
2. **Intrinsic value**: the payoff from exercising now

$$V_j = \\max(\\text{intrinsic}, e^{-r\\Delta t}(p \\cdot V_j^{\\text{up}} + (1-p) \\cdot V_j^{\\text{down}}))$$

For a put, intrinsic value = max(K - S, 0).

The option holder will exercise early if the intrinsic value exceeds the continuation value.

### Early Exercise Premium

The **early exercise premium** is the difference between the American and European prices:

$$\\text{Early Exercise Premium} = \\text{American Price} - \\text{European Price}$$

This is always ≥ 0 (American options are worth at least as much as European).`,
  starterCode: `import math

def american_put_binomial(S, K, T, r, sigma, n):
    """Price an American put option using an n-step binomial tree with early exercise."""
    pass`,
  solution: `import math

def american_put_binomial(S, K, T, r, sigma, n):
    """Price an American put option using an n-step binomial tree with early exercise."""
    dt = T / n
    u = math.exp(sigma * math.sqrt(dt))
    d = 1 / u
    p = (math.exp(r * dt) - d) / (u - d)
    disc = math.exp(-r * dt)
    # Terminal payoffs
    values = [max(K - S * u**(n - 2*j), 0) for j in range(n + 1)]
    # Backward induction with early exercise
    for i in range(n - 1, -1, -1):
        stock_prices = [S * u**(i - 2*j) for j in range(i + 1)]
        values = [max(K - stock_prices[j], disc * (p * values[j] + (1 - p) * values[j + 1])) for j in range(i + 1)]
    return values[0]`,
  tests: [
    {
      name: "American put (S=K=100, T=1, r=0.05, sigma=0.2, n=50)",
      code: `{{FUNC}}\nprint(round(american_put_binomial(100, 100, 1, 0.05, 0.2, 50), 4))`,
      expected: "6.0737\n",
    },
    {
      name: "American put (S=K=100, T=1, r=0.05, sigma=0.2, n=100)",
      code: `{{FUNC}}\nprint(round(american_put_binomial(100, 100, 1, 0.05, 0.2, 100), 4))`,
      expected: "6.0824\n",
    },
    {
      name: "American put >= European put (early exercise premium >= 0)",
      code: `{{FUNC}}\nimport math\ndef norm_cdf(x):\n    return 0.5 * math.erfc(-x / math.sqrt(2))\ndef bs_put(S, K, T, r, sigma):\n    d1 = (math.log(S/K) + (r + sigma**2/2)*T) / (sigma*math.sqrt(T))\n    d2 = d1 - sigma*math.sqrt(T)\n    return K*math.exp(-r*T)*norm_cdf(-d2) - S*norm_cdf(-d1)\namerican = american_put_binomial(100, 100, 1, 0.05, 0.2, 50)\neuropean = bs_put(100, 100, 1, 0.05, 0.2)\nprint(american >= european)`,
      expected: "True\n",
    },
    {
      name: "early exercise premium is positive for ITM put",
      code: `{{FUNC}}\nimport math\ndef norm_cdf(x):\n    return 0.5 * math.erfc(-x / math.sqrt(2))\ndef bs_put(S, K, T, r, sigma):\n    d1 = (math.log(S/K) + (r + sigma**2/2)*T) / (sigma*math.sqrt(T))\n    d2 = d1 - sigma*math.sqrt(T)\n    return K*math.exp(-r*T)*norm_cdf(-d2) - S*norm_cdf(-d1)\namerican = american_put_binomial(100, 100, 1, 0.05, 0.2, 50)\neuropean = bs_put(100, 100, 1, 0.05, 0.2)\npremium = american - european\nprint(round(premium, 4) > 0)`,
      expected: "True\n",
    },
  ],
};
