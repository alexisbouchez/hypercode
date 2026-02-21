import type { Lesson } from "../../types";

export const discountFactors: Lesson = {
  id: "discount-factors",
  title: "Discount Factors",
  chapterId: "yield-curves",
  content: `## Discount Factors

A **discount factor** $d(t)$ is the present value of $1 received at time $t$. It is the building block for pricing all fixed-income instruments.

### Discrete Compounding

$$d(t) = \\frac{1}{(1+r)^t}$$

### Continuous Compounding

$$d(t) = e^{-rt}$$

### Relationship to Spot Rates

The discount factor and spot rate are equivalent representations of the same information:
- Given $d(t)$, the spot rate is: $s = d(t)^{-1/t} - 1$
- Given $s$, the discount factor is: $d(t) = 1/(1+s)^t$

### Applications

Any fixed-income price can be written as:

$$P = \\sum_t CF_t \\cdot d(t)$$

Discount factors provide a model-free way to price instruments once the yield curve is known.

### Example

At 5% annual rate:
- $d(1) = 1/1.05 \\approx 0.9524$
- $d(5) = 1/1.05^5 \\approx 0.7835$
- $d_{cont}(1) = e^{-0.05} \\approx 0.9512$`,
  starterCode: `import math

def discount_factor(r, t):
    # Discrete discount factor: 1/(1+r)^t
    pass

def discount_factor_cont(r, t):
    # Continuous discount factor: exp(-r*t)
    pass`,
  solution: `import math

def discount_factor(r, t):
    return 1 / (1 + r) ** t

def discount_factor_cont(r, t):
    return math.exp(-r * t)`,
  tests: [
    {
      name: "discount_factor(0.05, 1) ≈ 0.9524",
      code: `{{FUNC}}\nprint(round(discount_factor(0.05, 1), 4))`,
      expected: "0.9524\n",
    },
    {
      name: "discount_factor(0.05, 5) ≈ 0.7835",
      code: `{{FUNC}}\nprint(round(discount_factor(0.05, 5), 4))`,
      expected: "0.7835\n",
    },
    {
      name: "discount_factor_cont(0.05, 1) ≈ 0.9512",
      code: `{{FUNC}}\nprint(round(discount_factor_cont(0.05, 1), 4))`,
      expected: "0.9512\n",
    },
    {
      name: "discount_factor_cont(0.05, 5) ≈ 0.7788",
      code: `{{FUNC}}\nprint(round(discount_factor_cont(0.05, 5), 4))`,
      expected: "0.7788\n",
    },
  ],
};
