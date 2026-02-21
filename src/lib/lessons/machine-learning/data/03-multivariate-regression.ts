import type { Lesson } from "../../types";

export const multivariateRegression: Lesson = {
	id: "multivariate-regression",
	title: "Multivariate Regression",
	chapterId: "supervised",
	content: `## Multivariate Regression

Real datasets have multiple features. **Multivariate linear regression** extends the single-variable case to $d$ input features:

$$\\hat{y} = \\mathbf{w} \\cdot \\mathbf{x} + b = \\sum_{j=1}^{d} w_j x_j + b$$

where $\\mathbf{w} = [w_1, \\ldots, w_d]$ is the weight vector and $\\mathbf{x} = [x_1, \\ldots, x_d]$ is the feature vector.

### Dot Product

The core operation is the **dot product**:

$$\\mathbf{w} \\cdot \\mathbf{x} = \\sum_{j=1}^{d} w_j x_j$$

### Multivariate MSE

For a dataset $\\{(\\mathbf{x}^{(i)}, y^{(i)})\\}_{i=1}^n$:

$$\\text{MSE} = \\frac{1}{n} \\sum_{i=1}^{n} \\left( \\hat{y}^{(i)} - y^{(i)} \\right)^2$$

where each $\\hat{y}^{(i)} = \\mathbf{w} \\cdot \\mathbf{x}^{(i)} + b$.

### Your Task

Implement:
- \`dot_product(x, w)\` — sum of element-wise products
- \`predict_multi(x, w, b)\` — dot product plus bias
- \`mse_multi(X, w, b, y_true)\` — MSE over a dataset of feature vectors`,

	starterCode: `def dot_product(x, w):
    # Sum of x[i] * w[i]
    return 0.0

def predict_multi(x, w, b):
    # dot_product(x, w) + b
    return 0.0

def mse_multi(X, w, b, y_true):
    # Mean squared error over rows of X
    return 0.0

print(dot_product([1.0, 2.0, 3.0], [4.0, 5.0, 6.0]))  # 32.0
print(predict_multi([1.0, 2.0], [3.0, 4.0], 1.0))      # 12.0
X = [[1.0, 2.0], [3.0, 4.0]]
print(mse_multi(X, [1.0, 1.0], 0.0, [3.0, 7.0]))       # 0.0
`,

	solution: `def dot_product(x, w):
    return sum(xi * wi for xi, wi in zip(x, w))

def predict_multi(x, w, b):
    return dot_product(x, w) + b

def mse_multi(X, w, b, y_true):
    y_pred = [predict_multi(x, w, b) for x in X]
    return sum((p - t) ** 2 for p, t in zip(y_pred, y_true)) / len(y_true)

print(dot_product([1.0, 2.0, 3.0], [4.0, 5.0, 6.0]))
print(predict_multi([1.0, 2.0], [3.0, 4.0], 1.0))
X = [[1.0, 2.0], [3.0, 4.0]]
print(mse_multi(X, [1.0, 1.0], 0.0, [3.0, 7.0]))
`,

	tests: [
		{
			name: "dot=32.0, predict_multi=12.0, mse_multi perfect=0.0",
			expected: "32.0\n12.0\n0.0\n",
		},
		{
			name: "mse_multi with error = 1.0",
			code: `{{FUNC}}
X = [[1.0, 0.0], [0.0, 1.0]]
print(mse_multi(X, [1.0, 1.0], 0.0, [2.0, 2.0]))`,
			expected: "1.0\n",
		},
		{
			name: "dot_product orthogonal vectors = 0.0",
			code: `{{FUNC}}
print(dot_product([1.0, 0.0], [0.0, 1.0]))`,
			expected: "0.0\n",
		},
		{
			name: "predict_multi with zero bias",
			code: `{{FUNC}}
print(predict_multi([2.0, 3.0], [1.0, 1.0], 0.0))`,
			expected: "5.0\n",
		},
		{
			name: "dot_product single element",
			code: `{{FUNC}}
print(dot_product([7.0], [3.0]))`,
			expected: "21.0\n",
		},
	],
};
