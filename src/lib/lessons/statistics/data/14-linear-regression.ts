import type { Lesson } from "../../types";

export const linearRegression: Lesson = {
	id: "linear-regression",
	title: "Linear Regression",
	chapterId: "regression",
	content: `## Fitting a Line

**Linear regression** finds the best-fit line through data points:

\`\`\`
y = slope · x + intercept
\`\`\`

\`\`\`python
def linear_regression(x, y):
    n = len(x)
    sx, sy = sum(x), sum(y)
    sxy = sum(xi*yi for xi, yi in zip(x, y))
    sxx = sum(xi**2 for xi in x)

    slope = (n*sxy - sx*sy) / (n*sxx - sx**2)
    intercept = (sy - slope*sx) / n
    return slope, intercept

x = [0, 1, 2, 3, 4]
y = [1, 3, 5, 7, 9]  # y = 2x + 1

slope, intercept = linear_regression(x, y)
print(round(slope, 4))      # 2.0
print(round(intercept, 4))  # 1.0
\`\`\`

### Least Squares

The formula minimizes the **sum of squared residuals** — the vertical distances between data points and the line.

### R² (Coefficient of Determination)

R² measures how much variance in y is explained by x:
- **R² = 1.0** — perfect fit, all points on the line
- **R² = 0.0** — the line explains nothing
- **R² = 0.8** — the line explains 80% of the variance

\`\`\`python
mean_y = sy / n
yhat = [slope*xi + intercept for xi in x]
ss_res = sum((yi - yhi)**2 for yi, yhi in zip(y, yhat))
ss_tot = sum((yi - mean_y)**2 for yi in y)
r_sq = 1 - ss_res / ss_tot
\`\`\`

### Assumptions

Linear regression assumes:
1. Linear relationship between x and y
2. Homoscedasticity (equal variance of residuals)
3. Independent observations
4. Approximately normal residuals

### Your Task

Implement \`linear_regression(x, y)\` that prints the **slope**, **intercept**, and **R²** (coefficient of determination), each rounded to 4 decimal places.`,

	starterCode: `def linear_regression(x, y):
    # Print slope, intercept, and R², each rounded to 4 decimal places
    pass

linear_regression([0, 1, 2, 3, 4], [1, 3, 5, 7, 9])
`,

	solution: `def linear_regression(x, y):
    n = len(x)
    sx, sy = sum(x), sum(y)
    sxy = sum(xi*yi for xi, yi in zip(x, y))
    sxx = sum(xi**2 for xi in x)
    slope = (n*sxy - sx*sy) / (n*sxx - sx**2)
    intercept = (sy - slope*sx) / n
    mean_y = sy / n
    yhat = [slope*xi + intercept for xi in x]
    ss_res = sum((yi - yhi)**2 for yi, yhi in zip(y, yhat))
    ss_tot = sum((yi - mean_y)**2 for yi in y)
    r_sq = 1 - ss_res / ss_tot if ss_tot != 0 else 1.0
    print(round(slope, 4))
    print(round(intercept, 4))
    print(round(r_sq, 4))

linear_regression([0, 1, 2, 3, 4], [1, 3, 5, 7, 9])
`,

	tests: [
		{
			name: "linear_regression([0..4], [1,3,5,7,9]) → slope=2.0, intercept=1.0, R²=1.0",
			expected: "2.0\n1.0\n1.0\n",
		},
		{
			name: "linear_regression([1..5], [2,4,6,8,10]) → slope=2.0, intercept=0.0, R²=1.0",
			code: `{{FUNC}}
linear_regression([1, 2, 3, 4, 5], [2, 4, 6, 8, 10])`,
			expected: "2.0\n0.0\n1.0\n",
		},
		{
			name: "slope is negative for decreasing data",
			code: `{{FUNC}}
x, y = [1,2,3,4,5], [10,8,6,4,2]
n = len(x)
sx, sy = sum(x), sum(y)
sxy = sum(xi*yi for xi,yi in zip(x,y))
sxx = sum(xi**2 for xi in x)
slope = (n*sxy - sx*sy) / (n*sxx - sx**2)
print(slope < 0)`,
			expected: "True\n",
		},
		{
			name: "R² is between 0 and 1",
			code: `{{FUNC}}
x, y = [1,2,3,4,5], [2,3,5,4,6]
n = len(x)
sx, sy = sum(x), sum(y)
sxy = sum(xi*yi for xi,yi in zip(x,y))
sxx = sum(xi**2 for xi in x)
slope = (n*sxy - sx*sy) / (n*sxx - sx**2)
intercept = (sy - slope*sx) / n
mean_y = sy / n
yhat = [slope*xi + intercept for xi in x]
ss_res = sum((yi-yhi)**2 for yi,yhi in zip(y,yhat))
ss_tot = sum((yi-mean_y)**2 for yi in y)
r_sq = 1 - ss_res/ss_tot
print(0 <= r_sq <= 1)`,
			expected: "True\n",
		},
	],
};
