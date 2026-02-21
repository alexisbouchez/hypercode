import type { Lesson } from "../../types";

export const bsDelta: Lesson = {
  id: "bs-delta",
  title: "Delta (Call & Put)",
  chapterId: "black-scholes",
  content: `## Delta: The First Greek

**Delta** (Δ) measures how much the option price changes for a $1 move in the underlying stock price:

$$\\Delta = \\frac{\\partial V}{\\partial S}$$

### Black-Scholes Delta

For a European call:
$$\\Delta_{\\text{call}} = N(d_1)$$

For a European put:
$$\\Delta_{\\text{put}} = N(d_1) - 1$$

Since $0 \\leq N(d_1) \\leq 1$:
- Call delta is always between 0 and 1
- Put delta is always between -1 and 0

### Interpretation

- Delta ≈ 0.6 means: the call gains ~$0.60 for every $1 the stock rises
- An ATM option has delta ≈ 0.5 (call) or ≈ -0.5 (put)
- A deep ITM call has delta ≈ 1 (moves dollar-for-dollar with the stock)
- A deep OTM call has delta ≈ 0 (barely reacts to stock moves)

### Delta as Probability

Delta is also approximately equal to the risk-neutral probability that the option expires in-the-money.

### Delta Hedging

If you sell 1 call option (delta = 0.6), you can hedge by buying 0.6 shares of stock. This creates a **delta-neutral** portfolio that is insensitive to small stock moves.`,
  starterCode: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def bs_delta_call(S, K, T, r, sigma):
    """Delta of a European call option."""
    pass

def bs_delta_put(S, K, T, r, sigma):
    """Delta of a European put option."""
    pass`,
  solution: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def bs_delta_call(S, K, T, r, sigma):
    """Delta of a European call option."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    return norm_cdf(d1)

def bs_delta_put(S, K, T, r, sigma):
    """Delta of a European put option."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    return norm_cdf(d1) - 1`,
  tests: [
    {
      name: "call delta ATM (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_delta_call(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "0.6368\n",
    },
    {
      name: "put delta ATM (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_delta_put(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "-0.3632\n",
    },
    {
      name: "call delta ITM (S=110, K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_delta_call(110, 100, 1, 0.05, 0.2), 4))`,
      expected: "0.7958\n",
    },
    {
      name: "put delta OTM (S=90, K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_delta_put(90, 100, 1, 0.05, 0.2), 4))`,
      expected: "-0.5702\n",
    },
  ],
};
