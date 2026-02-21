import type { Lesson } from "../../types";

export const logArithmeticReturns: Lesson = {
  id: "log-arithmetic-returns",
  title: "Log vs Arithmetic Returns",
  chapterId: "return-analysis",
  content: `## Log vs Arithmetic Returns

In finance, there are two common ways to compute returns between two prices.

**Arithmetic Return** (simple return) measures the percentage change:

$$r = \\frac{P_1 - P_0}{P_0}$$

**Log Return** (continuously compounded) uses the natural logarithm:

$$r_{log} = \\ln\\left(\\frac{P_1}{P_0}\\right)$$

Log returns are preferred in statistical analysis because they are additive over time: the log return over multiple periods equals the sum of individual log returns. Arithmetic returns are more intuitive for single-period reporting.

For small returns, the two are approximately equal. For larger moves, they diverge — log returns are always slightly smaller in magnitude.

### Your Task

Implement both return functions:
- \`arithmetic_return(p0, p1)\` — returns \`(p1 - p0) / p0\`
- \`log_return(p0, p1)\` — returns \`math.log(p1 / p0)\``,
  starterCode: `import math

def arithmetic_return(p0, p1):
    # Compute (p1 - p0) / p0
    pass

def log_return(p0, p1):
    # Compute math.log(p1 / p0)
    pass`,
  solution: `import math

def arithmetic_return(p0, p1):
    return (p1 - p0) / p0

def log_return(p0, p1):
    return math.log(p1 / p0)`,
  tests: [
    {
      name: "arithmetic_return(100, 110) equals 0.1",
      code: `{{FUNC}}
print(round(arithmetic_return(100, 110), 4))`,
      expected: "0.1\n",
    },
    {
      name: "log_return(100, 110) equals ~0.0953",
      code: `{{FUNC}}
print(round(log_return(100, 110), 4))`,
      expected: "0.0953\n",
    },
    {
      name: "log_return(100, 90) equals ~-0.1054",
      code: `{{FUNC}}
print(round(log_return(100, 90), 4))`,
      expected: "-0.1054\n",
    },
  ],
};
