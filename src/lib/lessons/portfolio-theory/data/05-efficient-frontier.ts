import type { Lesson } from "../../types";

export const efficientFrontier: Lesson = {
  id: "efficient-frontier",
  title: "Efficient Frontier",
  chapterId: "optimization",
  content: `## The Efficient Frontier

The **efficient frontier** is the set of portfolios that offer the highest expected return for a given level of risk (or equivalently, the lowest risk for a given return).

For two assets, we can trace out the entire frontier by sweeping the weight w₁ from 0 to 1 (with w₂ = 1 − w₁) and computing:
- Portfolio standard deviation: σ_p(w₁)
- Portfolio expected return: μ_p(w₁)

The resulting curve in (σ, μ) space is the **mean-variance frontier**.

### Example

With σ₁ = 0.10, σ₂ = 0.20, ρ = 0.30, μ₁ = 0.08, μ₂ = 0.12:

- At w₁ = 0 (all asset 2): (σ, μ) = (0.20, 0.12)
- At w₁ = 1 (all asset 1): (σ, μ) = (0.10, 0.08)

### Your Task

Implement \`efficient_frontier_2(s1, s2, corr, mu1, mu2, n=20)\` that returns a list of \`n+1\` tuples \`(std, ret)\`, evenly spaced from w₁ = 0 to w₁ = 1. Round each value to 6 decimal places.`,
  starterCode: `import math

def efficient_frontier_2(s1, s2, corr, mu1, mu2, n=20):
    pass`,
  solution: `import math

def efficient_frontier_2(s1, s2, corr, mu1, mu2, n=20):
    result = []
    for i in range(n + 1):
        w1 = i / n
        w2 = 1 - w1
        var = w1**2 * s1**2 + w2**2 * s2**2 + 2 * w1 * w2 * corr * s1 * s2
        std = math.sqrt(var)
        ret = w1 * mu1 + w2 * mu2
        result.append((round(std, 6), round(ret, 6)))
    return result`,
  tests: [
    {
      name: "first point (w1=0, all asset 2)",
      code: `{{FUNC}}\npts = efficient_frontier_2(0.1, 0.2, 0.3, 0.08, 0.12, 20)\nprint(pts[0])`,
      expected: "(0.2, 0.12)\n",
    },
    {
      name: "last point (w1=1, all asset 1)",
      code: `{{FUNC}}\npts = efficient_frontier_2(0.1, 0.2, 0.3, 0.08, 0.12, 20)\nprint(pts[-1])`,
      expected: "(0.1, 0.08)\n",
    },
    {
      name: "midpoint (w1=0.5)",
      code: `{{FUNC}}\npts = efficient_frontier_2(0.1, 0.2, 0.3, 0.08, 0.12, 20)\nprint(pts[10])`,
      expected: "(0.124499, 0.1)\n",
    },
    {
      name: "number of points returned",
      code: `{{FUNC}}\npts = efficient_frontier_2(0.1, 0.2, 0.3, 0.08, 0.12, 20)\nprint(len(pts))`,
      expected: "21\n",
    },
  ],
};
