import type { Lesson } from "../../types";

export const l2Regularization: Lesson = {
	id: "l2-regularization",
	title: "L2 Regularization",
	chapterId: "training",
	content: `## Preventing Overfitting

A network trained long enough on small data will **memorise** the training set — achieving near-zero training loss while generalising poorly. This is **overfitting**.

**Regularization** adds a penalty for large weights to the loss function, pushing the model toward simpler solutions.

### L2 Regularization (Weight Decay)

Add the squared norm of all weights to the loss:

$$\\mathcal{L}_{\\text{reg}} = \\mathcal{L}_{\\text{data}} + \\lambda \\sum_{i} w_i^2$$

$\\lambda$ (lambda) controls the trade-off between fitting the data and keeping weights small.

### Effect on Gradients

The regularization term adds an extra gradient contribution for each weight:

$$\\frac{\\partial \\mathcal{L}_{\\text{reg}}}{\\partial w_i} = \\frac{\\partial \\mathcal{L}_{\\text{data}}}{\\partial w_i} + 2\\lambda w_i$$

This pulls every weight toward zero on every update step — equivalent to multiplying weights by $(1 - 2\\lambda\\eta)$ before the gradient update, hence the name **weight decay**.

### Intuition

L2 regularization prefers many small weights over a few large ones. Geometrically, it constrains the weights to lie near the origin.

### Your Task

Implement:
- \`l2_loss(predictions, targets, weights_flat, lambda_)\` — MSE plus L2 penalty
- \`l2_grad(weight, lambda_)\` — gradient of $\\lambda w^2$ with respect to $w$`,

	starterCode: `def l2_loss(predictions, targets, weights_flat, lambda_=0.01):
    n = len(predictions)
    mse = sum((p - t) ** 2 for p, t in zip(predictions, targets)) / n
    # l2_penalty = lambda_ * sum(w**2 for w in weights_flat)
    l2_penalty = 0.0
    return mse + l2_penalty

def l2_grad(weight, lambda_=0.01):
    # Gradient of lambda * w^2 is 2 * lambda * w
    return 0.0

preds = [0.9, 0.1, 0.8]
targets = [1.0, 0.0, 1.0]
weights = [0.5, -0.3, 0.8]
print(round(l2_loss(preds, targets, weights, lambda_=0.1), 4))  # 0.118
print(round(l2_grad(0.5, lambda_=0.1), 4))                      # 0.1
`,

	solution: `def l2_loss(predictions, targets, weights_flat, lambda_=0.01):
    n = len(predictions)
    mse = sum((p - t) ** 2 for p, t in zip(predictions, targets)) / n
    l2_penalty = lambda_ * sum(w ** 2 for w in weights_flat)
    return mse + l2_penalty

def l2_grad(weight, lambda_=0.01):
    return 2 * lambda_ * weight

preds = [0.9, 0.1, 0.8]
targets = [1.0, 0.0, 1.0]
weights = [0.5, -0.3, 0.8]
print(round(l2_loss(preds, targets, weights, lambda_=0.1), 4))
print(round(l2_grad(0.5, lambda_=0.1), 4))
`,

	tests: [
		{
			name: "l2_loss and l2_grad basic values",
			expected: "0.118\n0.1\n",
		},
		{
			name: "zero lambda gives plain MSE",
			code: `{{FUNC}}
preds = [0.5, 0.5]
targets = [0.0, 1.0]
mse_only = l2_loss(preds, targets, [100.0, 200.0], lambda_=0.0)
print(round(mse_only, 4))`,
			expected: "0.25\n",
		},
		{
			name: "l2_grad is proportional to weight",
			code: `{{FUNC}}
print(round(l2_grad(1.0, lambda_=0.5), 4))
print(round(l2_grad(2.0, lambda_=0.5), 4))`,
			expected: "1.0\n2.0\n",
		},
		{
			name: "negative weight has negative gradient",
			code: `{{FUNC}}
print(round(l2_grad(-0.5, lambda_=0.1), 4))`,
			expected: "-0.1\n",
		},
	],
};
