import type { Lesson } from "../../types";

export const linearRegression: Lesson = {
	id: "linear-regression",
	title: "Linear Regression",
	chapterId: "supervised",
	content: `## Linear Regression

**Linear regression** is the simplest supervised learning model. It learns a linear mapping from an input $x$ to an output $y$:

$$\\hat{y} = wx + b$$

where $w$ is the **weight** (slope) and $b$ is the **bias** (intercept).

### Mean Squared Error

To measure how well the model fits the data, we use the **Mean Squared Error (MSE)**:

$$\\text{MSE} = \\frac{1}{n} \\sum_{i=1}^{n} (\\hat{y}_i - y_i)^2$$

A perfect model has MSE = 0. Higher MSE means worse predictions.

### R-Squared

The **coefficient of determination** $R^2$ measures how much variance in $y$ is explained by the model:

$$R^2 = 1 - \\frac{SS_{\\text{res}}}{SS_{\\text{tot}}}$$

where:
- $SS_{\\text{res}} = \\sum (\\hat{y}_i - y_i)^2$ — residual sum of squares
- $SS_{\\text{tot}} = \\sum (y_i - \\bar{y})^2$ — total sum of squares

$R^2 = 1$ means a perfect fit; $R^2 = 0$ means the model is no better than predicting the mean.

### Your Task

Implement:
- \`predict(x, w, b)\` — returns $wx + b$
- \`mse_loss(y_pred, y_true)\` — mean of squared differences
- \`r_squared(y_pred, y_true)\` — coefficient of determination`,

	starterCode: `def predict(x, w, b):
    # Return w*x + b
    return 0.0

def mse_loss(y_pred, y_true):
    # Mean squared error
    return 0.0

def r_squared(y_pred, y_true):
    # Coefficient of determination
    return 0.0

print(predict(2, 3, 1))                          # 7
print(mse_loss([2.0, 3.0], [1.0, 3.0]))          # 0.5
print(round(r_squared([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]), 4))  # 1.0
`,

	solution: `def predict(x, w, b):
    return w * x + b

def mse_loss(y_pred, y_true):
    n = len(y_pred)
    return sum((p - t) ** 2 for p, t in zip(y_pred, y_true)) / n

def r_squared(y_pred, y_true):
    y_mean = sum(y_true) / len(y_true)
    ss_res = sum((p - t) ** 2 for p, t in zip(y_pred, y_true))
    ss_tot = sum((t - y_mean) ** 2 for t in y_true)
    return 1 - ss_res / ss_tot

print(predict(2, 3, 1))
print(mse_loss([2.0, 3.0], [1.0, 3.0]))
print(round(r_squared([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]), 4))
`,

	tests: [
		{
			name: "predict(2,3,1)=7, mse([2,3],[1,3])=0.5, r2 perfect=1.0",
			expected: "7\n0.5\n1.0\n",
		},
		{
			name: "predict(0, 5, 2) = 2",
			code: `{{FUNC}}
print(predict(0, 5, 2))`,
			expected: "2\n",
		},
		{
			name: "mse_loss identical predictions = 0.0",
			code: `{{FUNC}}
print(mse_loss([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]))`,
			expected: "0.0\n",
		},
		{
			name: "r_squared constant predictions = 0.0",
			code: `{{FUNC}}
print(round(r_squared([2.0, 2.0, 2.0], [1.0, 2.0, 3.0]), 4))`,
			expected: "0.0\n",
		},
		{
			name: "predict(-3, 2, 5) = -1",
			code: `{{FUNC}}
print(predict(-3, 2, 5))`,
			expected: "-1\n",
		},
	],
};
