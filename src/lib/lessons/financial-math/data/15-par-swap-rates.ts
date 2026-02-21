import type { Lesson } from "../../types";

export const parSwapRates: Lesson = {
  id: "par-swap-rates",
  title: "Par Rates & Swap Rates",
  chapterId: "yield-curves",
  content: `## Par Rates & Swap Rates

### Par Rate

The **par rate** for maturity $n$ is the coupon rate that makes a bond price exactly equal to its face value (price = 1):

$$1 = p_n \\sum_{t=1}^{n} d(t) + d(n)$$

Solving for $p_n$:

$$p_n = \\frac{1 - d(n)}{\\sum_{t=1}^{n} d(t)}$$

Where $d(t) = \\frac{1}{(1 + s_t)^t}$ is the discount factor at time $t$.

### Swap Rate

An **interest rate swap** exchanges fixed payments for floating rate payments. The **swap rate** is the fixed rate that makes the swap's NPV equal to zero.

The swap rate formula is identical to the par rate formula:

$$\\text{Swap Rate} = \\frac{1 - d(n)}{\\sum_{t=1}^{n} d(t)}$$

### Intuition

Both formulas have the same structure because a fixed-rate bond and a swap have the same cash flow structure at inception:
- Numerator: $1 - d(n)$ = the net difference between the initial notional and the final discounted payment
- Denominator: sum of discount factors = the annuity factor

A rising yield curve means longer swap rates exceed shorter ones.`,
  starterCode: `def par_rate(spot_rates):
    # Compute the par rate given a list of spot rates [s1, s2, ..., sn]
    # d(t) = 1/(1+s_t)^t
    # par = (1 - d(n)) / sum(d(t))
    pass

def swap_rate(spot_rates):
    # Fixed rate making swap NPV = 0 (same formula as par rate)
    pass`,
  solution: `def par_rate(spot_rates):
    n = len(spot_rates)
    dfs = [1 / (1 + s) ** (i + 1) for i, s in enumerate(spot_rates)]
    return (1 - dfs[-1]) / sum(dfs)

def swap_rate(spot_rates):
    return par_rate(spot_rates)`,
  tests: [
    {
      name: "par_rate([0.03, 0.035, 0.04]) ≈ 0.0397",
      code: `{{FUNC}}\nprint(round(par_rate([0.03, 0.035, 0.04]), 4))`,
      expected: "0.0397\n",
    },
    {
      name: "swap_rate([0.04, 0.045, 0.05, 0.055]) ≈ 0.0543",
      code: `{{FUNC}}\nprint(round(swap_rate([0.04, 0.045, 0.05, 0.055]), 4))`,
      expected: "0.0543\n",
    },
    {
      name: "par_rate([0.05, 0.055, 0.06]) ≈ 0.0596",
      code: `{{FUNC}}\nprint(round(par_rate([0.05, 0.055, 0.06]), 4))`,
      expected: "0.0596\n",
    },
    {
      name: "swap_rate([0.03, 0.04, 0.05]) ≈ 0.0493",
      code: `{{FUNC}}\nprint(round(swap_rate([0.03, 0.04, 0.05]), 4))`,
      expected: "0.0493\n",
    },
  ],
};
