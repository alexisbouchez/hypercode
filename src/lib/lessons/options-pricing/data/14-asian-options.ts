import type { Lesson } from "../../types";

export const asianOptions: Lesson = {
  id: "asian-options",
  title: "Asian Options (Path-Dependent)",
  chapterId: "numerical-methods",
  content: `## Asian Options

**Asian options** are path-dependent derivatives where the payoff depends on the **average price** of the underlying over the life of the option, rather than just the final price.

### Why Asian Options?

- **Reduced manipulation risk**: harder to manipulate the average than a single price at expiry
- **Smoothed exposure**: useful when a company's cash flows accumulate over time (e.g., exporters)
- **Lower cost**: average price reduces volatility → lower option price than vanilla

### Arithmetic Average Asian Call

The payoff of an arithmetic average Asian call is:

$$\\text{Payoff} = \\max\\left(\\bar{S} - K, 0\\right)$$

where $\\bar{S} = \\frac{1}{n} \\sum_{i=1}^{n} S_{t_i}$ is the arithmetic average of stock prices at observation times.

### Monte Carlo Algorithm

For each simulated path:
1. Simulate n_steps stock prices along the path
2. Compute the arithmetic average of all prices
3. Calculate the payoff: max(avg - K, 0)

Then discount the average payoff:
$$C_{\\text{Asian}} \\approx e^{-rT} \\cdot \\text{mean payoff}$$

### Comparison to Vanilla

Asian options are always cheaper than (or equal to) vanilla calls with the same strike, because the average price is less volatile than the terminal price.`,
  starterCode: `import math
import random

def asian_call_price(S, K, T, r, sigma, n_steps, n_paths, seed):
    """Price an arithmetic average Asian call using Monte Carlo."""
    pass`,
  solution: `import math
import random

def asian_call_price(S, K, T, r, sigma, n_steps, n_paths, seed):
    """Price an arithmetic average Asian call using Monte Carlo."""
    rng = random.Random(seed)
    dt = T / n_steps
    disc = math.exp(-r * T)
    total = 0.0
    for _ in range(n_paths):
        St = S
        path_sum = 0.0
        for _ in range(n_steps):
            u1 = rng.random()
            u2 = rng.random()
            z = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
            St = St * math.exp((r - sigma**2 / 2) * dt + sigma * math.sqrt(dt) * z)
            path_sum += St
        avg = path_sum / n_steps
        total += max(avg - K, 0)
    return disc * total / n_paths`,
  tests: [
    {
      name: "Asian call ATM (S=K=100, T=1, r=0.05, sigma=0.2, steps=50, paths=5000, seed=42)",
      code: `{{FUNC}}\nprint(round(asian_call_price(100, 100, 1, 0.05, 0.2, 50, 5000, 42), 4))`,
      expected: "5.8623\n",
    },
    {
      name: "Asian call with lower strike (S=100, K=95, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(asian_call_price(100, 95, 1, 0.05, 0.2, 50, 5000, 42), 4))`,
      expected: "8.9263\n",
    },
    {
      name: "Asian call higher vol (sigma=0.3)",
      code: `{{FUNC}}\nprint(round(asian_call_price(100, 100, 1, 0.05, 0.3, 50, 5000, 42), 4))`,
      expected: "8.0814\n",
    },
    {
      name: "Asian call is cheaper than vanilla call (same params)",
      code: `{{FUNC}}\nimport math\ndef norm_cdf(x):\n    return 0.5 * math.erfc(-x / math.sqrt(2))\ndef bs_call(S, K, T, r, sigma):\n    d1 = (math.log(S/K) + (r + sigma**2/2)*T) / (sigma*math.sqrt(T))\n    d2 = d1 - sigma*math.sqrt(T)\n    return S*norm_cdf(d1) - K*math.exp(-r*T)*norm_cdf(d2)\nasian = asian_call_price(100, 100, 1, 0.05, 0.2, 50, 5000, 42)\nvanilla = bs_call(100, 100, 1, 0.05, 0.2)\nprint(asian < vanilla)`,
      expected: "True\n",
    },
  ],
};
