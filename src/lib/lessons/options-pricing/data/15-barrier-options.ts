import type { Lesson } from "../../types";

export const barrierOptions: Lesson = {
  id: "barrier-options",
  title: "Barrier Options",
  chapterId: "numerical-methods",
  content: `## Barrier Options

**Barrier options** are path-dependent options that are either activated or cancelled if the underlying price hits a specified barrier level.

### Types of Barrier Options

| Type | Activated/cancelled when... |
|------|---------------------------|
| Down-and-out | Price falls below barrier → option cancelled (knocked out) |
| Down-and-in | Price falls below barrier → option activated (knocked in) |
| Up-and-out | Price rises above barrier → option cancelled |
| Up-and-in | Price rises above barrier → option activated |

### Down-and-Out Call

The most common barrier option. It behaves like a vanilla call, but is **cancelled** if the stock price ever touches or drops below the barrier B (where B < S).

$$\\text{Payoff} = \\begin{cases} \\max(S_T - K, 0) & \\text{if } S_t > B \\text{ for all } t \\\\ 0 & \\text{if } S_t \\leq B \\text{ for any } t \\end{cases}$$

### Monte Carlo Algorithm

For each path:
1. Simulate step by step
2. At each step, check if the price has hit or crossed the barrier
3. If knocked out, the payoff is 0
4. Otherwise, payoff = max(S_T - K, 0)

### Effect of the Barrier

- Barrier **far below** current price: rarely triggered → price ≈ vanilla call
- Barrier **close to** current price: frequently triggered → price < vanilla call
- As B → 0: down-and-out call → vanilla call
- As B → S: down-and-out call → 0 (immediately knocked out)`,
  starterCode: `import math
import random

def down_and_out_call(S, K, B, T, r, sigma, n_steps, n_paths, seed):
    """Price a down-and-out call option using Monte Carlo simulation.
    
    B: barrier level (B < S, option knocked out if price touches B)
    """
    pass`,
  solution: `import math
import random

def down_and_out_call(S, K, B, T, r, sigma, n_steps, n_paths, seed):
    """Price a down-and-out call option using Monte Carlo simulation.
    
    B: barrier level (B < S, option knocked out if price touches B)
    """
    rng = random.Random(seed)
    dt = T / n_steps
    disc = math.exp(-r * T)
    total = 0.0
    for _ in range(n_paths):
        St = S
        knocked_out = False
        for _ in range(n_steps):
            u1 = rng.random()
            u2 = rng.random()
            z = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
            St = St * math.exp((r - sigma**2 / 2) * dt + sigma * math.sqrt(dt) * z)
            if St <= B:
                knocked_out = True
                break
        if not knocked_out:
            total += max(St - K, 0)
    return disc * total / n_paths`,
  tests: [
    {
      name: "barrier far below S (B=50): price close to vanilla",
      code: `{{FUNC}}\nprint(round(down_and_out_call(100, 100, 50, 1, 0.05, 0.2, 50, 5000, 42), 4))`,
      expected: "10.4332\n",
    },
    {
      name: "barrier closer to S (B=90): price lower than vanilla",
      code: `{{FUNC}}\nprint(round(down_and_out_call(100, 100, 90, 1, 0.05, 0.2, 50, 5000, 42), 4))`,
      expected: "9.3728\n",
    },
    {
      name: "barrier far below gives higher price than barrier close to S",
      code: `{{FUNC}}\nfar = down_and_out_call(100, 100, 50, 1, 0.05, 0.2, 100, 10000, 123)\nclose = down_and_out_call(100, 100, 90, 1, 0.05, 0.2, 100, 10000, 123)\nprint(far > close)`,
      expected: "True\n",
    },
    {
      name: "barrier option price is always non-negative",
      code: `{{FUNC}}\nprice = down_and_out_call(100, 100, 80, 1, 0.05, 0.2, 50, 5000, 42)\nprint(price >= 0)`,
      expected: "True\n",
    },
  ],
};
