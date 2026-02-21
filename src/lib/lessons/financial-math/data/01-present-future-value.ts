import type { Lesson } from "../../types";

export const presentFutureValue: Lesson = {
  id: "present-future-value",
  title: "Present & Future Value",
  chapterId: "tvm",
  content: `## Present & Future Value

The **time value of money** is the foundational principle of finance: a dollar today is worth more than a dollar in the future.

### Future Value

If you invest \\$PV today at interest rate \\$r\\$ per period for \\$n\\$ periods:

$$FV = PV \\cdot (1 + r)^n$$

### Present Value

To find what a future amount is worth today (discounting):

$$PV = \\frac{FV}{(1 + r)^n}$$

The factor $\\frac{1}{(1+r)^n}$ is called the **discount factor**.

### Example

Invest $1000 at 5% annually for 3 years:
- FV = 1000 × (1.05)³ = $1157.63

What is $1000 received in 3 years worth today at 5%?
- PV = 1000 / (1.05)³ = $863.84

Implement both functions using Python's \`**\` exponentiation operator.`,
  starterCode: `def pv(fv, r, n):
    # Present value: discount FV back n periods at rate r
    pass

def fv(pv, r, n):
    # Future value: grow PV forward n periods at rate r
    pass`,
  solution: `def pv(fv, r, n):
    return fv / (1 + r) ** n

def fv(pv, r, n):
    return pv * (1 + r) ** n`,
  tests: [
    {
      name: "pv(1000, 0.05, 3) ≈ 863.8376",
      code: `{{FUNC}}\nprint(round(pv(1000, 0.05, 3), 4))`,
      expected: "863.8376\n",
    },
    {
      name: "fv(1000, 0.05, 3) ≈ 1157.625",
      code: `{{FUNC}}\nprint(round(fv(1000, 0.05, 3), 4))`,
      expected: "1157.625\n",
    },
    {
      name: "pv(500, 0.1, 5) ≈ 310.4607",
      code: `{{FUNC}}\nprint(round(pv(500, 0.1, 5), 4))`,
      expected: "310.4607\n",
    },
    {
      name: "pv(2000, 0.08, 10) ≈ 926.387",
      code: `{{FUNC}}\nprint(round(pv(2000, 0.08, 10), 4))`,
      expected: "926.387\n",
    },
  ],
};
