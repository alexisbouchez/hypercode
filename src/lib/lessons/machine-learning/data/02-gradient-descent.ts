import type { Lesson } from "../../types";

export const gradientDescent: Lesson = {
	id: "gradient-descent",
	title: "Gradient Descent",
	chapterId: "supervised",
	content: `## Gradient Descent

**Gradient descent** is the optimization algorithm that trains linear regression. It iteratively adjusts $w$ and $b$ to minimise the MSE loss.

### The Gradients

Given predictions $\\hat{y}_i = wx_i + b$ and true labels $y_i$, the gradients of MSE with respect to $w$ and $b$ are:

$$\\frac{\\partial \\text{MSE}}{\\partial w} = \\frac{1}{n} \\sum_{i=1}^{n} (\\hat{y}_i - y_i) x_i$$

$$\\frac{\\partial \\text{MSE}}{\\partial b} = \\frac{1}{n} \\sum_{i=1}^{n} (\\hat{y}_i - y_i)$$

### Parameter Update

After computing the gradients, each parameter is updated by a small step in the **negative** gradient direction:

$$w \\leftarrow w - \\alpha \\cdot \\nabla_w$$

$$b \\leftarrow b - \\alpha \\cdot \\nabla_b$$

where $\\alpha$ is the **learning rate** — a small positive number like $0.01$.

### Training Loop

Repeat for many iterations:
1. Compute predictions $\\hat{y}$
2. Compute gradients $\\nabla_w$, $\\nabla_b$
3. Update $w$ and $b$

### Your Task

Implement:
- \`gradient_w(x, y_pred, y_true)\` — gradient of MSE w.r.t. $w$
- \`gradient_b(y_pred, y_true)\` — gradient of MSE w.r.t. $b$
- \`update_param(param, grad, lr)\` — one gradient descent step`,

	starterCode: `def gradient_w(x, y_pred, y_true):
    # Mean of (y_pred[i] - y_true[i]) * x[i]
    return 0.0

def gradient_b(y_pred, y_true):
    # Mean of (y_pred[i] - y_true[i])
    return 0.0

def update_param(param, grad, lr):
    # param - lr * grad
    return 0.0

x = [1.0, 2.0, 3.0]
y_pred = [2.0, 4.0, 6.0]
y_true = [1.0, 2.0, 3.0]
print(round(gradient_w(x, y_pred, y_true), 4))   # 4.6667
print(round(gradient_b(y_pred, y_true), 4))       # 2.0
print(round(update_param(0.5, 0.1, 0.01), 4))     # 0.499
`,

	solution: `def gradient_w(x, y_pred, y_true):
    n = len(y_pred)
    return sum((y_pred[i] - y_true[i]) * x[i] for i in range(n)) / n

def gradient_b(y_pred, y_true):
    n = len(y_pred)
    return sum(y_pred[i] - y_true[i] for i in range(n)) / n

def update_param(param, grad, lr):
    return param - lr * grad

x = [1.0, 2.0, 3.0]
y_pred = [2.0, 4.0, 6.0]
y_true = [1.0, 2.0, 3.0]
print(round(gradient_w(x, y_pred, y_true), 4))
print(round(gradient_b(y_pred, y_true), 4))
print(round(update_param(0.5, 0.1, 0.01), 4))
`,

	tests: [
		{
			name: "gradient_w=4.6667, gradient_b=2.0, update_param=0.499",
			expected: "4.6667\n2.0\n0.499\n",
		},
		{
			name: "update_param(1.0, -0.5, 0.1) = 1.05 (negative grad increases param)",
			code: `{{FUNC}}
print(round(update_param(1.0, -0.5, 0.1), 4))`,
			expected: "1.05\n",
		},
		{
			name: "gradient_b when predictions equal truth = 0.0",
			code: `{{FUNC}}
print(gradient_b([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]))`,
			expected: "0.0\n",
		},
		{
			name: "gradient_w when predictions equal truth = 0.0",
			code: `{{FUNC}}
print(gradient_w([1.0, 2.0], [3.0, 4.0], [3.0, 4.0]))`,
			expected: "0.0\n",
		},
		{
			name: "update_param(2.0, 0.4, 0.5) = 1.8",
			code: `{{FUNC}}
print(round(update_param(2.0, 0.4, 0.5), 4))`,
			expected: "1.8\n",
		},
	],
};
