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
from scipy import stats

x = [0, 1, 2, 3, 4]
y = [1, 3, 5, 7, 9]  # y = 2x + 1

result = stats.linregress(x, y)
print(round(result.slope, 4))      # 2.0
print(round(result.intercept, 4))  # 1.0
print(round(result.rvalue ** 2, 4))  # 1.0 — R² (perfect fit)
\`\`\`

### Least Squares

scipy finds the line that **minimizes the sum of squared residuals** — the vertical distances between data points and the line.

### R² (Coefficient of Determination)

R² measures how much variance in y is explained by x:
- **R² = 1.0** — perfect fit, all points on the line
- **R² = 0.0** — the line explains nothing
- **R² = 0.8** — the line explains 80% of the variance

\`\`\`python
print(result.rvalue ** 2)   # R² = rvalue squared
\`\`\`

### Prediction

Once you have the slope and intercept, you can predict:
\`\`\`python
def predict(x_new):
    return result.slope * x_new + result.intercept

print(predict(10))   # 21.0 for our example
\`\`\`

### Assumptions

Linear regression assumes:
1. Linear relationship between x and y
2. Homoscedasticity (equal variance of residuals)
3. Independent observations
4. Approximately normal residuals

### Your Task

Implement \`linear_regression(x, y)\` that prints the **slope**, **intercept**, and **R²** (coefficient of determination), each rounded to 4 decimal places.`,

	starterCode: `from scipy import stats

def linear_regression(x, y):
    # Print slope, intercept, and R² (rvalue**2), each rounded to 4 decimal places
    pass

linear_regression([0, 1, 2, 3, 4], [1, 3, 5, 7, 9])
`,

	solution: `from scipy import stats

def linear_regression(x, y):
    result = stats.linregress(x, y)
    print(round(float(result.slope), 4))
    print(round(float(result.intercept), 4))
    print(round(float(result.rvalue ** 2), 4))

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
from scipy import stats
result = stats.linregress([1,2,3,4,5], [10,8,6,4,2])
print(result.slope < 0)`,
			expected: "True\n",
		},
		{
			name: "R² is between 0 and 1",
			code: `{{FUNC}}
from scipy import stats
result = stats.linregress([1,2,3,4,5], [2,3,5,4,6])
r_sq = result.rvalue ** 2
print(0 <= r_sq <= 1)`,
			expected: "True\n",
		},
	],
};
