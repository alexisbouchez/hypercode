import type { Lesson } from "../../types";

export const rSquared: Lesson = {
  id: "r-squared",
  title: "R-squared & Residuals",
  chapterId: "regression",
  content: `## R-squared & Residuals

After fitting an OLS line, we need to assess how well it fits the data.

**Residuals** are the differences between actual and predicted values:
$$e_i = y_i - \\hat{y}_i = y_i - (\\beta x_i + \\alpha)$$

**R-squared** (coefficient of determination) measures the proportion of variance explained:
$$R^2 = 1 - \\frac{SS_{res}}{SS_{tot}}$$

where:
- $SS_{res} = \\sum (y_i - \\hat{y}_i)^2$ — residual sum of squares
- $SS_{tot} = \\sum (y_i - \\bar{y})^2$ — total sum of squares

$R^2 = 1$ means perfect fit; $R^2 = 0$ means the regression explains nothing (as good as just predicting the mean). In factor models, $R^2$ represents how much of an asset's variance is explained by the factors.

### Your Task

Implement:
- \`r_squared(xs, ys)\` — coefficient of determination using OLS fit
- \`residuals(xs, ys)\` — list of OLS residuals`,
  starterCode: `def ols(xs, ys):
    n = len(xs)
    mx = sum(xs) / n
    my = sum(ys) / n
    ss_xy = sum(xs[i] * ys[i] for i in range(n)) - n * mx * my
    ss_xx = sum(x ** 2 for x in xs) - n * mx ** 2
    slope = ss_xy / ss_xx
    intercept = my - slope * mx
    return (slope, intercept)

def r_squared(xs, ys):
    # 1 - SS_res / SS_tot
    pass

def residuals(xs, ys):
    # y_i - (slope * x_i + intercept) for each i
    pass`,
  solution: `def ols(xs, ys):
    n = len(xs)
    mx = sum(xs) / n
    my = sum(ys) / n
    ss_xy = sum(xs[i] * ys[i] for i in range(n)) - n * mx * my
    ss_xx = sum(x ** 2 for x in xs) - n * mx ** 2
    slope = ss_xy / ss_xx
    intercept = my - slope * mx
    return (slope, intercept)

def r_squared(xs, ys):
    n = len(xs)
    my = sum(ys) / n
    slope, intercept = ols(xs, ys)
    y_pred = [slope * x + intercept for x in xs]
    ss_res = sum((ys[i] - y_pred[i]) ** 2 for i in range(n))
    ss_tot = sum((y - my) ** 2 for y in ys)
    return 1 - ss_res / ss_tot

def residuals(xs, ys):
    slope, intercept = ols(xs, ys)
    return [ys[i] - (slope * xs[i] + intercept) for i in range(len(xs))]`,
  tests: [
    {
      name: "r_squared([1,2,3,4,5], [2,4,5,4,5]) equals 0.6",
      code: `{{FUNC}}
print(round(r_squared([1,2,3,4,5], [2,4,5,4,5]), 4))`,
      expected: "0.6\n",
    },
    {
      name: "residuals([1,2,3,4,5], [2,4,5,4,5]) correct values",
      code: `{{FUNC}}
print([round(r, 4) for r in residuals([1,2,3,4,5], [2,4,5,4,5])])`,
      expected: "[-0.8, 0.6, 1.0, -0.6, -0.2]\n",
    },
    {
      name: "r_squared of perfect linear fit equals 1.0",
      code: `{{FUNC}}
print(round(r_squared([1,2,3,4,5], [3,5,7,9,11]), 4))`,
      expected: "1.0\n",
    },
  ],
};
