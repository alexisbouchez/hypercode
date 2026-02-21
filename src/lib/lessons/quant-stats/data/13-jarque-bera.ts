import type { Lesson } from "../../types";

export const jarqueBera: Lesson = {
  id: "jarque-bera",
  title: "Jarque-Bera Test",
  chapterId: "regression",
  content: `## Jarque-Bera Test

The **Jarque-Bera (JB) test** checks whether a sample comes from a normal distribution by examining skewness and excess kurtosis. A normal distribution has skewness = 0 and excess kurtosis = 0.

The JB statistic is:
$$JB = \\frac{n}{6} \\left( S^2 + \\frac{K^2}{4} \\right)$$

where:
- $n$ is the sample size
- $S$ is the sample skewness: $\\frac{\\frac{1}{n}\\sum(x_i - \\bar{x})^3}{\\hat{\\sigma}^3}$
- $K$ is the excess kurtosis: $\\frac{\\frac{1}{n}\\sum(x_i - \\bar{x})^4}{\\hat{\\sigma}^4} - 3$
- $\\hat{\\sigma}$ is the **population** standard deviation (divide by $n$)

Under the null hypothesis of normality, $JB$ follows a chi-squared distribution with 2 degrees of freedom. Large $JB$ values reject normality.

Symmetric data ($S = 0$) and mesokurtic data ($K = 0$) yield small JB. Skewed or heavy-tailed data yield large JB.

### Your Task

Implement \`jarque_bera(xs)\` returning the JB statistic.`,
  starterCode: `import math

def jarque_bera(xs):
    # JB = n/6 * (skew^2 + kurtosis^2 / 4)
    # Use population std for normalization
    pass`,
  solution: `import math

def jarque_bera(xs):
    n = len(xs)
    m = sum(xs) / n
    pop_std = math.sqrt(sum((x - m) ** 2 for x in xs) / n)
    skew = (1 / n) * sum((x - m) ** 3 for x in xs) / pop_std ** 3
    kurt = (1 / n) * sum((x - m) ** 4 for x in xs) / pop_std ** 4 - 3
    return n / 6 * (skew ** 2 + kurt ** 2 / 4)`,
  tests: [
    {
      name: "jarque_bera([1..10]) is small (near-symmetric, ~0.6245)",
      code: `{{FUNC}}
print(round(jarque_bera([1,2,3,4,5,6,7,8,9,10]), 4))`,
      expected: "0.6245\n",
    },
    {
      name: "jarque_bera of skewed data is large (~18.7604)",
      code: `{{FUNC}}
print(round(jarque_bera([1,1,1,1,2,2,3,10,20,100]), 4))`,
      expected: "18.7604\n",
    },
    {
      name: "jarque_bera is non-negative",
      code: `{{FUNC}}
result = jarque_bera([3, 3, 3, 3, 3, 3, 4, 4, 4, 5])
print(result >= 0)`,
      expected: "True\n",
    },
  ],
};
