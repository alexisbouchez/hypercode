import type { Lesson } from "../../types";

export const forwardRates: Lesson = {
  id: "forward-rates",
  title: "Forward Rates",
  chapterId: "interest-rate-models",
  content: `## Forward Rates

A **forward rate** is the implied future interest rate between two time periods, derived from current spot rates.

### No-Arbitrage Derivation

An investor can either:
1. Invest for $t_2$ years at spot rate $s_2$
2. Invest for $t_1$ years at $s_1$, then roll over at forward rate $f$

No-arbitrage requires both strategies to produce the same result:

$$(1 + s_2)^{t_2} = (1 + s_1)^{t_1} \\cdot (1 + f)^{t_2 - t_1}$$

### Forward Rate Formula

$$f(t_1, t_2) = \\left(\\frac{(1 + s_2)^{t_2}}{(1 + s_1)^{t_1}}\\right)^{\\frac{1}{t_2 - t_1}} - 1$$

### Example

Spot rates: 1-year = 3%, 2-year = 4%

$$f(1, 2) = \\frac{1.04^2}{1.03^1} - 1 = \\frac{1.0816}{1.03} - 1 \\approx 5.01\\%$$

The market implies a 1-year rate of 5.01% starting one year from now.

### Interpretation

If the forward rate exceeds current short rates, the market expects rates to rise.`,
  starterCode: `def forward_rate(s1, s2, t1, t2):
    # Implied forward rate from t1 to t2, given spot rates s1 (at t1) and s2 (at t2)
    pass`,
  solution: `def forward_rate(s1, s2, t1, t2):
    return ((1 + s2) ** t2 / (1 + s1) ** t1) ** (1 / (t2 - t1)) - 1`,
  tests: [
    {
      name: "forward_rate(0.03, 0.04, 1, 2) ≈ 0.0501",
      code: `{{FUNC}}\nprint(round(forward_rate(0.03, 0.04, 1, 2), 4))`,
      expected: "0.0501\n",
    },
    {
      name: "forward_rate(0.04, 0.05, 2, 3) ≈ 0.0703",
      code: `{{FUNC}}\nprint(round(forward_rate(0.04, 0.05, 2, 3), 4))`,
      expected: "0.0703\n",
    },
    {
      name: "forward_rate(0.03, 0.05, 1, 3) ≈ 0.0601",
      code: `{{FUNC}}\nprint(round(forward_rate(0.03, 0.05, 1, 3), 4))`,
      expected: "0.0601\n",
    },
    {
      name: "forward_rate(0.05, 0.06, 3, 5) ≈ 0.0752",
      code: `{{FUNC}}\nprint(round(forward_rate(0.05, 0.06, 3, 5), 4))`,
      expected: "0.0752\n",
    },
  ],
};
