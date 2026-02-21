import type { Lesson } from "../../types";

export const annuities: Lesson = {
  id: "annuities",
  title: "Annuities & Perpetuities",
  chapterId: "tvm",
  content: `## Annuities & Perpetuities

An **annuity** is a series of equal payments made at regular intervals. A **perpetuity** pays forever.

### Annuity Present Value

For payments of PMT per period over n periods at rate r:

$$PV_{annuity} = PMT \\cdot \\frac{1 - (1+r)^{-n}}{r}$$

This formula sums the geometric series of discounted cash flows:
$$PV = \\sum_{t=1}^{n} \\frac{PMT}{(1+r)^t}$$

### Perpetuity Present Value

When n → ∞, the annuity formula simplifies to:

$$PV_{perpetuity} = \\frac{PMT}{r}$$

### Examples

- Annuity: $100/year for 5 years at 5% → PV = $432.95
- Perpetuity: $100/year forever at 5% → PV = $2000

These are used to value bonds, loans, leases, and dividend-paying stocks.`,
  starterCode: `def annuity_pv(pmt, r, n):
    # Present value of annuity: PMT payments over n periods at rate r
    pass

def perpetuity_pv(pmt, r):
    # Present value of perpetuity: PMT payments forever at rate r
    pass`,
  solution: `def annuity_pv(pmt, r, n):
    return pmt * (1 - (1 + r) ** -n) / r

def perpetuity_pv(pmt, r):
    return pmt / r`,
  tests: [
    {
      name: "annuity_pv(100, 0.05, 5) ≈ 432.9477",
      code: `{{FUNC}}\nprint(round(annuity_pv(100, 0.05, 5), 4))`,
      expected: "432.9477\n",
    },
    {
      name: "perpetuity_pv(100, 0.05) = 2000.0",
      code: `{{FUNC}}\nprint(round(perpetuity_pv(100, 0.05), 4))`,
      expected: "2000.0\n",
    },
    {
      name: "annuity_pv(200, 0.08, 10) ≈ 1342.0163",
      code: `{{FUNC}}\nprint(round(annuity_pv(200, 0.08, 10), 4))`,
      expected: "1342.0163\n",
    },
    {
      name: "annuity_pv(50, 0.03, 20) ≈ 743.8737",
      code: `{{FUNC}}\nprint(round(annuity_pv(50, 0.03, 20), 4))`,
      expected: "743.8737\n",
    },
  ],
};
