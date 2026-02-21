import type { Lesson } from "../../types";

export const npv: Lesson = {
  id: "npv",
  title: "Net Present Value",
  chapterId: "tvm",
  content: `## Net Present Value

**Net Present Value (NPV)** measures the value of an investment by summing all discounted cash flows, including the initial outlay.

$$NPV = \\sum_{t=0}^{n} \\frac{CF_t}{(1+r)^t}$$

Where:
- $CF_t$ is the cash flow at time $t$
- $CF_0$ is typically negative (initial investment)
- $r$ is the discount rate (required return)

### Decision Rule

- **NPV > 0**: Investment creates value — accept
- **NPV < 0**: Investment destroys value — reject
- **NPV = 0**: Investment earns exactly the required return

### Example

Project with initial cost $1000 and returns of $300, $400, $500 at 10%:

$$NPV = -1000 + \\frac{300}{1.1} + \\frac{400}{1.1^2} + \\frac{500}{1.1^3} = -21.04$$

This project returns less than the 10% required rate, so it should be rejected.

Use Python's \`enumerate\` to iterate with index for the time periods.`,
  starterCode: `def npv(rate, cashflows):
    # Net present value: sum of discounted cash flows
    # cashflows[0] is at time 0 (typically the initial investment, negative)
    pass`,
  solution: `def npv(rate, cashflows):
    return sum(cf / (1 + rate) ** t for t, cf in enumerate(cashflows))`,
  tests: [
    {
      name: "npv(0.1, [-1000, 300, 400, 500]) ≈ -21.0368",
      code: `{{FUNC}}\nprint(round(npv(0.1, [-1000, 300, 400, 500]), 4))`,
      expected: "-21.0368\n",
    },
    {
      name: "npv(0.1, [-1000, 200, 200, 200]) is negative",
      code: `{{FUNC}}\nprint(round(npv(0.1, [-1000, 200, 200, 200]), 4))`,
      expected: "-502.6296\n",
    },
    {
      name: "npv(0.05, [-500, 100, 200, 300]) ≈ 35.7953",
      code: `{{FUNC}}\nprint(round(npv(0.05, [-500, 100, 200, 300]), 4))`,
      expected: "35.7953\n",
    },
  ],
};
