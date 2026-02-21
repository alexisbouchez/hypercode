import type { Lesson } from "../../types";

export const covarianceCorrelation: Lesson = {
  id: "covariance-correlation",
  title: "Covariance & Correlation",
  chapterId: "return-analysis",
  content: `## Covariance & Correlation

**Covariance** measures how two variables move together. A positive covariance means they tend to move in the same direction; negative means opposite.

Sample covariance between $X$ and $Y$:
$$\\text{Cov}(X, Y) = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})(y_i - \\bar{y})$$

**Pearson Correlation** normalizes covariance to the range $[-1, 1]$:
$$\\rho = \\frac{\\text{Cov}(X, Y)}{s_X \\cdot s_Y}$$

where $s_X$ and $s_Y$ are the sample standard deviations.

A correlation of 1 means perfect positive linear relationship, -1 means perfect inverse, and 0 means no linear relationship.

In portfolio construction, correlation determines diversification benefit: assets with low or negative correlation reduce portfolio volatility.

### Your Task

Implement:
- \`covariance(xs, ys)\` — sample covariance
- \`correlation(xs, ys)\` — Pearson correlation coefficient`,
  starterCode: `import math

def covariance(xs, ys):
    # Sample covariance: sum((x - mean_x)(y - mean_y)) / (n - 1)
    pass

def correlation(xs, ys):
    # Pearson correlation: covariance(xs, ys) / (std_x * std_y)
    pass`,
  solution: `import math

def covariance(xs, ys):
    n = len(xs)
    mx = sum(xs) / n
    my = sum(ys) / n
    return sum((xs[i] - mx) * (ys[i] - my) for i in range(n)) / (n - 1)

def correlation(xs, ys):
    n = len(xs)
    mx = sum(xs) / n
    my = sum(ys) / n
    std_x = math.sqrt(sum((x - mx) ** 2 for x in xs) / (n - 1))
    std_y = math.sqrt(sum((y - my) ** 2 for y in ys) / (n - 1))
    return covariance(xs, ys) / (std_x * std_y)`,
  tests: [
    {
      name: "covariance([1,2,3,4,5], [2,4,3,5,6]) equals 2.25",
      code: `{{FUNC}}
print(round(covariance([1,2,3,4,5], [2,4,3,5,6]), 4))`,
      expected: "2.25\n",
    },
    {
      name: "correlation([1,2,3,4,5], [2,4,3,5,6]) equals 0.9",
      code: `{{FUNC}}
print(round(correlation([1,2,3,4,5], [2,4,3,5,6]), 4))`,
      expected: "0.9\n",
    },
    {
      name: "correlation of perfectly correlated series equals 1.0",
      code: `{{FUNC}}
print(round(correlation([1,2,3,4,5], [2,4,6,8,10]), 4))`,
      expected: "1.0\n",
    },
    {
      name: "covariance of identical lists equals variance",
      code: `{{FUNC}}
xs = [1,2,3,4,5]
print(round(covariance(xs, xs), 4))`,
      expected: "2.5\n",
    },
  ],
};
