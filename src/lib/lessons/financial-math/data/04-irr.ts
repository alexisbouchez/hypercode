import type { Lesson } from "../../types";

export const irr: Lesson = {
  id: "irr",
  title: "Internal Rate of Return",
  chapterId: "tvm",
  content: `## Internal Rate of Return

The **Internal Rate of Return (IRR)** is the discount rate that makes an investment's NPV equal to zero:

$$\\sum_{t=0}^{n} \\frac{CF_t}{(1+IRR)^t} = 0$$

### Finding IRR with Bisection

There is no closed-form solution for IRR in general. We use **bisection search**:

1. Set bounds: \`lo = -0.9999\`, \`hi = 10.0\`
2. Compute midpoint: \`mid = (lo + hi) / 2\`
3. If \`npv(mid, cashflows) > 0\`: IRR is higher → \`lo = mid\`
4. If \`npv(mid, cashflows) < 0\`: IRR is lower → \`hi = mid\`
5. Repeat ~1000 times for convergence

### Decision Rule

- **IRR > hurdle rate**: Accept the investment
- **IRR < hurdle rate**: Reject the investment

### Example

For cash flows [-1000, 300, 400, 500]: IRR ≈ 8.9%

This means the investment earns about 8.9% annually. If your required return is 10%, reject it (matches our NPV result).`,
  starterCode: `def npv(rate, cashflows):
    return sum(cf / (1 + rate) ** t for t, cf in enumerate(cashflows))

def irr(cashflows):
    # Find the rate where npv(rate, cashflows) == 0 using bisection
    # Use lo=-0.9999, hi=10.0, iterate 1000 times
    pass`,
  solution: `def npv(rate, cashflows):
    return sum(cf / (1 + rate) ** t for t, cf in enumerate(cashflows))

def irr(cashflows):
    lo, hi = -0.9999, 10.0
    for _ in range(1000):
        mid = (lo + hi) / 2
        val = npv(mid, cashflows)
        if abs(val) < 1e-10:
            break
        if val > 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2`,
  tests: [
    {
      name: "irr([-1000, 300, 400, 500]) ≈ 0.089",
      code: `{{FUNC}}\nprint(round(irr([-1000, 300, 400, 500]), 4))`,
      expected: "0.089\n",
    },
    {
      name: "irr([-1000, 200, 300, 600]) ≈ 0.0414",
      code: `{{FUNC}}\nprint(round(irr([-1000, 200, 300, 600]), 4))`,
      expected: "0.0414\n",
    },
    {
      name: "irr([-500, 100, 200, 300]) ≈ 0.0821",
      code: `{{FUNC}}\nprint(round(irr([-500, 100, 200, 300]), 4))`,
      expected: "0.0821\n",
    },
  ],
};
