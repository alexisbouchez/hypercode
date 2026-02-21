import type { Lesson } from "../../types";

export const bootstrapping: Lesson = {
  id: "bootstrapping",
  title: "Bootstrapping Yield Curve",
  chapterId: "interest-rate-models",
  content: `## Bootstrapping the Yield Curve

**Bootstrapping** extracts spot rates from par bond prices iteratively. A **par bond** is priced at face value (price = 1), so its coupon rate equals its YTM.

### Algorithm

For a par bond maturing in year $n$ with par rate $p_n$, and normalized face value = 1:

$$1 = p_n \\sum_{t=1}^{n-1} d_t + (1 + p_n) \\cdot d_n$$

Where $d_t = \\frac{1}{(1+s_t)^t}$ is the discount factor at time $t$.

Solving for $d_n$ (the unknown discount factor for year $n$):

$$d_n = \\frac{1 - p_n \\sum_{t=1}^{n-1} d_t}{1 + p_n}$$

Then: $s_n = d_n^{-1/n} - 1$

### Step-by-Step

1. Year 1: $s_1 = p_1$ (trivially, the 1-year par rate is the 1-year spot rate)
2. Year 2: Use $s_1$ to compute $d_1$, solve for $d_2$, then $s_2$
3. Year 3: Use $s_1, s_2$ to compute $d_1, d_2$, solve for $d_3$, then $s_3$
4. Continue iterating...`,
  starterCode: `def bootstrap_spot_rates(par_rates):
    # Given a list of annual par rates [p1, p2, ...],
    # return a list of spot rates [s1, s2, ...]
    # Year 1: spot rate = par rate
    # Year n: solve for discount factor using known spot rates
    pass`,
  solution: `def bootstrap_spot_rates(par_rates):
    spot_rates = []
    for i, par in enumerate(par_rates):
        n = i + 1
        if i == 0:
            spot_rates.append(par)
        else:
            sum_dfs = sum(1 / (1 + spot_rates[j]) ** (j + 1) for j in range(i))
            df_n = (1 - par * sum_dfs) / (1 + par)
            spot_n = (1 / df_n) ** (1 / n) - 1
            spot_rates.append(spot_n)
    return spot_rates`,
  tests: [
    {
      name: "bootstrap [0.03, 0.035, 0.04] → [0.03, 0.0351, 0.0403]",
      code: `{{FUNC}}\nresult = bootstrap_spot_rates([0.03, 0.035, 0.04])\nfor r in result:\n    print(round(r, 4))`,
      expected: "0.03\n0.0351\n0.0403\n",
    },
    {
      name: "bootstrap [0.04, 0.045, 0.05, 0.055] → [0.04, 0.0451, 0.0503, 0.0557]",
      code: `{{FUNC}}\nresult = bootstrap_spot_rates([0.04, 0.045, 0.05, 0.055])\nfor r in result:\n    print(round(r, 4))`,
      expected: "0.04\n0.0451\n0.0503\n0.0557\n",
    },
    {
      name: "bootstrap [0.05] → [0.05] (single rate)",
      code: `{{FUNC}}\nresult = bootstrap_spot_rates([0.05])\nfor r in result:\n    print(round(r, 4))`,
      expected: "0.05\n",
    },
  ],
};
