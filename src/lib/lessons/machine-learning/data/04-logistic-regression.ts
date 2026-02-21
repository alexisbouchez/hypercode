import type { Lesson } from "../../types";

export const logisticRegression: Lesson = {
	id: "logistic-regression",
	title: "Logistic Regression",
	chapterId: "supervised",
	content: `## Logistic Regression

**Logistic regression** is the standard model for binary classification. Instead of predicting a continuous value, it predicts the probability that an example belongs to class 1.

### The Sigmoid Function

The **sigmoid** (logistic) function squashes any real number into the interval $(0, 1)$:

$$\\sigma(z) = \\frac{1}{1 + e^{-z}}$$

Key properties:
- $\\sigma(0) = 0.5$
- $\\sigma(z) \\to 1$ as $z \\to +\\infty$
- $\\sigma(z) \\to 0$ as $z \\to -\\infty$

### Logistic Prediction

$$\\hat{p} = \\sigma(wx + b) = \\frac{1}{1 + e^{-(wx+b)}}$$

### Binary Cross-Entropy Loss

For binary classification with labels $y_i \\in \\{0, 1\\}$, we minimise:

$$\\mathcal{L} = -\\frac{1}{n} \\sum_{i=1}^{n} \\left[ y_i \\log(\\hat{p}_i) + (1 - y_i) \\log(1 - \\hat{p}_i) \\right]$$

Use $\\varepsilon = 10^{-15}$ inside the logarithms to avoid $\\log(0)$.

### Your Task

Implement:
- \`sigmoid(z)\` — the logistic function
- \`logistic_predict(x, w, b)\` — sigmoid of the linear combination
- \`binary_cross_entropy(y_pred, y_true)\` — average cross-entropy loss`,

	starterCode: `import math

def sigmoid(z):
    # 1 / (1 + exp(-z))
    return 0.0

def logistic_predict(x, w, b):
    # sigmoid(w*x + b)
    return 0.0

def binary_cross_entropy(y_pred, y_true):
    # -mean(y_true*log(y_pred+eps) + (1-y_true)*log(1-y_pred+eps))
    return 0.0

print(round(sigmoid(0), 4))                              # 0.5
print(round(sigmoid(100), 4))                            # 1.0
print(round(logistic_predict(1.0, 2.0, -1.0), 4))       # 0.7311
print(round(binary_cross_entropy([0.5, 0.5], [1, 0]), 4))  # 0.6931
`,

	solution: `import math

def sigmoid(z):
    return 1 / (1 + math.exp(-z))

def logistic_predict(x, w, b):
    return sigmoid(w * x + b)

def binary_cross_entropy(y_pred, y_true):
    eps = 1e-15
    n = len(y_pred)
    return -sum(
        y_true[i] * math.log(y_pred[i] + eps) + (1 - y_true[i]) * math.log(1 - y_pred[i] + eps)
        for i in range(n)
    ) / n

print(round(sigmoid(0), 4))
print(round(sigmoid(100), 4))
print(round(logistic_predict(1.0, 2.0, -1.0), 4))
print(round(binary_cross_entropy([0.5, 0.5], [1, 0]), 4))
`,

	tests: [
		{
			name: "sigmoid(0)=0.5, sigmoid(100)=1.0, logistic_predict=0.7311, bce=0.6931",
			expected: "0.5\n1.0\n0.7311\n0.6931\n",
		},
		{
			name: "sigmoid(-100) = 0.0",
			code: `{{FUNC}}
print(round(sigmoid(-100), 4))`,
			expected: "0.0\n",
		},
		{
			name: "binary_cross_entropy near-perfect predictions ≈ 0.0",
			code: `{{FUNC}}
print(round(binary_cross_entropy([1.0, 0.0], [1, 0]), 4))`,
			expected: "-0.0\n",
		},
		{
			name: "logistic_predict(0, 1.0, 0.0) = 0.5",
			code: `{{FUNC}}
print(round(logistic_predict(0, 1.0, 0.0), 4))`,
			expected: "0.5\n",
		},
		{
			name: "sigmoid(1) ≈ 0.7311",
			code: `{{FUNC}}
print(round(sigmoid(1), 4))`,
			expected: "0.7311\n",
		},
	],
};
