import type { Lesson } from "../../types";

export const ols: Lesson = {
  id: "ols",
  title: "Linear Regression (OLS)",
  chapterId: "regression",
  content: `## Ordinary Least Squares (OLS)

**OLS regression** fits a line $\\hat{y} = \\beta x + \\alpha$ that minimizes the sum of squared residuals.

The closed-form solution for the **slope** is:
$$\\beta = \\frac{\\sum_{i=1}^{n}(x_i - \\bar{x})(y_i - \\bar{y})}{\\sum_{i=1}^{n}(x_i - \\bar{x})^2} = \\frac{SS_{xy}}{SS_{xx}}$$

And the **intercept**:
$$\\alpha = \\bar{y} - \\beta \\bar{x}$$

where $SS_{xy} = \\sum x_i y_i - n \\bar{x} \\bar{y}$ and $SS_{xx} = \\sum x_i^2 - n \\bar{x}^2$.

OLS is used everywhere in quantitative finance: factor models, risk attribution, and return prediction all rely on OLS regression.

### Your Task

Implement \`ols(xs, ys)\` which returns a tuple \`(slope, intercept)\`.`,
  starterCode: `def ols(xs, ys):
    # Returns (slope, intercept)
    # slope = SS_xy / SS_xx
    # intercept = mean_y - slope * mean_x
    pass`,
  solution: `def ols(xs, ys):
    n = len(xs)
    mx = sum(xs) / n
    my = sum(ys) / n
    ss_xy = sum(xs[i] * ys[i] for i in range(n)) - n * mx * my
    ss_xx = sum(x ** 2 for x in xs) - n * mx ** 2
    slope = ss_xy / ss_xx
    intercept = my - slope * mx
    return (slope, intercept)`,
  tests: [
    {
      name: "ols([1,2,3,4,5], [2,4,5,4,5]) slope equals 0.6",
      code: `{{FUNC}}
slope, intercept = ols([1,2,3,4,5], [2,4,5,4,5])
print(round(slope, 4))`,
      expected: "0.6\n",
    },
    {
      name: "ols([1,2,3,4,5], [2,4,5,4,5]) intercept equals 2.2",
      code: `{{FUNC}}
slope, intercept = ols([1,2,3,4,5], [2,4,5,4,5])
print(round(intercept, 4))`,
      expected: "2.2\n",
    },
    {
      name: "ols on perfectly linear data recovers exact slope",
      code: `{{FUNC}}
slope, intercept = ols([1,2,3,4,5], [3,5,7,9,11])
print(round(slope, 4))
print(round(intercept, 4))`,
      expected: "2.0\n1.0\n",
    },
  ],
};
