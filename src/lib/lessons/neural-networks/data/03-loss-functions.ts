import type { Lesson } from "../../types";

export const lossFunctions: Lesson = {
	id: "loss-functions",
	title: "Loss Functions",
	chapterId: "foundations",
	content: `## Measuring Error

A **loss function** (also called a cost function or objective) measures how wrong our network's predictions are. Training reduces this number. The choice of loss function depends on the task.

### Mean Squared Error (Regression)

For regression, we want predictions close to continuous target values:

$$\\mathcal{L}_{\\text{MSE}} = \\frac{1}{n} \\sum_{i=1}^{n} (\\hat{y}_i - y_i)^2$$

MSE penalizes large errors quadratically — a prediction off by 2 incurs 4× the loss of one off by 1.

### Binary Cross-Entropy (Classification)

For binary classification where $\\hat{y}_i \\in (0,1)$ is a probability:

$$\\mathcal{L}_{\\text{BCE}} = -\\frac{1}{n} \\sum_{i=1}^{n} \\left[ y_i \\log \\hat{y}_i + (1 - y_i) \\log(1 - \\hat{y}_i) \\right]$$

This is derived from maximum likelihood. When $y_i = 1$, only $\\log \\hat{y}_i$ matters — we want $\\hat{y}_i \\to 1$. When $y_i = 0$, only $\\log(1 - \\hat{y}_i)$ matters — we want $\\hat{y}_i \\to 0$.

The $\\varepsilon = 10^{-15}$ clip prevents $\\log(0) = -\\infty$.

### Your Task

Implement:
- \`mse(predictions, targets)\` — mean squared error
- \`binary_cross_entropy(predictions, targets)\` — binary cross-entropy with $\\varepsilon = 10^{-15}$`,

	starterCode: `import math

def mse(predictions, targets):
    # Mean of squared differences
    return 0.0

def binary_cross_entropy(predictions, targets):
    eps = 1e-15
    # -mean(y*log(p+eps) + (1-y)*log(1-p+eps))
    return 0.0

print(round(mse([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]), 4))   # 0.0
print(round(mse([0.0, 0.0], [1.0, 1.0]), 4))               # 1.0
print(round(binary_cross_entropy([0.9, 0.1], [1, 0]), 4))  # 0.1054
`,

	solution: `import math

def mse(predictions, targets):
    n = len(predictions)
    return sum((p - t) ** 2 for p, t in zip(predictions, targets)) / n

def binary_cross_entropy(predictions, targets):
    eps = 1e-15
    total = 0.0
    for p, t in zip(predictions, targets):
        total += -(t * math.log(p + eps) + (1 - t) * math.log(1 - p + eps))
    return total / len(predictions)

print(round(mse([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]), 4))
print(round(mse([0.0, 0.0], [1.0, 1.0]), 4))
print(round(binary_cross_entropy([0.9, 0.1], [1, 0]), 4))
`,

	tests: [
		{
			name: "mse perfect prediction, and binary cross-entropy",
			expected: "0.0\n1.0\n0.1054\n",
		},
		{
			name: "mse with partial error",
			code: `{{FUNC}}
print(round(mse([0.5, 0.5], [0.0, 1.0]), 4))`,
			expected: "0.25\n",
		},
		{
			name: "binary_cross_entropy perfect prediction is near zero",
			code: `{{FUNC}}
loss = binary_cross_entropy([0.9999, 0.0001], [1, 0])
print(loss < 0.001)`,
			expected: "True\n",
		},
		{
			name: "binary_cross_entropy wrong prediction is high",
			code: `{{FUNC}}
loss = binary_cross_entropy([0.01, 0.99], [1, 0])
print(loss > 4.0)`,
			expected: "True\n",
		},
	],
};
