import type { Lesson } from "../../types";

export const regularization: Lesson = {
	id: "regularization",
	title: "Regularization",
	chapterId: "evaluation",
	content: `## Regularization & Evaluation Metrics

**Overfitting** occurs when a model memorises the training data and fails to generalise. **Regularization** adds a penalty term to the loss function that discourages large weights.

### L1 Regularization (Lasso)

$$\\mathcal{L}_{\\text{L1}} = \\lambda \\sum_{i} |w_i|$$

L1 drives many weights to exactly zero, producing **sparse** models.

### L2 Regularization (Ridge)

$$\\mathcal{L}_{\\text{L2}} = \\lambda \\sum_{i} w_i^2$$

L2 shrinks all weights smoothly towards zero but rarely makes them exactly zero.

### Elastic Net

A combination of L1 and L2:

$$\\mathcal{L}_{\\text{EN}} = \\lambda_1 \\sum_i |w_i| + \\lambda_2 \\sum_i w_i^2$$

### Ridge Gradient

When L2 is added to the MSE loss, the gradient of $w_i$ gains an extra term:

$$\\nabla_{w_i} (\\text{MSE} + \\lambda \\|\\mathbf{w}\\|_2^2) = \\nabla_{w_i} \\text{MSE} + 2\\lambda w_i$$

This is why ridge regression is equivalent to **weight decay**: each weight is slightly shrunk at every update.

### Evaluation Metrics for Classification

Accuracy alone is misleading on imbalanced datasets (e.g., 99% negative class → a model that always predicts "negative" gets 99% accuracy). Instead, use the **confusion matrix** counts:

| | Predicted Positive | Predicted Negative |
|---|---|---|
| **Actually Positive** | TP (True Positive) | FN (False Negative) |
| **Actually Negative** | FP (False Positive) | TN (True Negative) |

From these we derive:

- **Precision**: Of all predicted positives, how many are correct? $P = \\frac{\\text{TP}}{\\text{TP} + \\text{FP}}$
- **Recall** (sensitivity): Of all actual positives, how many did we find? $R = \\frac{\\text{TP}}{\\text{TP} + \\text{FN}}$
- **F1 Score**: The harmonic mean of precision and recall: $F_1 = \\frac{2PR}{P + R}$

F1 balances the trade-off: high precision with low recall (conservative model) vs. high recall with low precision (aggressive model). Return 0.0 when both precision and recall are zero.

### Your Task

Implement:
- \`l1_penalty(w, lambda_)\` → $\\lambda \\sum |w_i|$
- \`l2_penalty(w, lambda_)\` → $\\lambda \\sum w_i^2$
- \`elastic_net(w, lambda1, lambda2)\` → L1 + L2
- \`ridge_gradient(w, grad_w, lambda_)\` → element-wise $\\nabla + 2\\lambda w$
- \`precision_recall_f1(y_true, y_pred)\` → tuple of (precision, recall, f1)`,

	starterCode: `def l1_penalty(w, lambda_):
    return 0.0

def l2_penalty(w, lambda_):
    return 0.0

def elastic_net(w, lambda1, lambda2):
    return 0.0

def ridge_gradient(w, grad_w, lambda_):
    return grad_w

def precision_recall_f1(y_true, y_pred):
    # Return (precision, recall, f1) for binary classification
    return (0.0, 0.0, 0.0)

w = [1.0, -2.0, 3.0]
print(round(l1_penalty(w, 0.1), 4))               # 0.6
print(round(l2_penalty(w, 0.1), 4))               # 1.4
print(round(elastic_net(w, 0.1, 0.1), 4))         # 2.0
gw = [0.1, 0.2, 0.3]
rg = ridge_gradient(w, gw, 0.1)
print([round(v, 4) for v in rg])                  # [0.3, -0.2, 0.9]
p, r, f1 = precision_recall_f1([1,1,0,0], [1,0,1,0])
print(round(p, 4), round(r, 4), round(f1, 4))     # 0.5 0.5 0.5
`,

	solution: `def l1_penalty(w, lambda_):
    return lambda_ * sum(abs(wi) for wi in w)

def l2_penalty(w, lambda_):
    return lambda_ * sum(wi ** 2 for wi in w)

def elastic_net(w, lambda1, lambda2):
    return l1_penalty(w, lambda1) + l2_penalty(w, lambda2)

def ridge_gradient(w, grad_w, lambda_):
    return [grad_w[i] + 2 * lambda_ * w[i] for i in range(len(w))]

def precision_recall_f1(y_true, y_pred):
    tp = sum(1 for yt, yp in zip(y_true, y_pred) if yt == 1 and yp == 1)
    fp = sum(1 for yt, yp in zip(y_true, y_pred) if yt == 0 and yp == 1)
    fn = sum(1 for yt, yp in zip(y_true, y_pred) if yt == 1 and yp == 0)
    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1 = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0.0
    return (precision, recall, f1)

w = [1.0, -2.0, 3.0]
print(round(l1_penalty(w, 0.1), 4))
print(round(l2_penalty(w, 0.1), 4))
print(round(elastic_net(w, 0.1, 0.1), 4))
gw = [0.1, 0.2, 0.3]
rg = ridge_gradient(w, gw, 0.1)
print([round(v, 4) for v in rg])
p, r, f1 = precision_recall_f1([1,1,0,0], [1,0,1,0])
print(round(p, 4), round(r, 4), round(f1, 4))
`,

	tests: [
		{
			name: "l1=0.6, l2=1.4, elastic_net=2.0, ridge_gradient, precision_recall_f1",
			expected: "0.6\n1.4\n2.0\n[0.3, -0.2, 0.9]\n0.5 0.5 0.5\n",
		},
		{
			name: "l1_penalty all-zero weights = 0.0",
			code: `{{FUNC}}
print(l1_penalty([0.0, 0.0, 0.0], 1.0))`,
			expected: "0.0\n",
		},
		{
			name: "l2_penalty unit weights",
			code: `{{FUNC}}
print(round(l2_penalty([1.0, 1.0, 1.0], 0.5), 4))`,
			expected: "1.5\n",
		},
		{
			name: "ridge_gradient no regularization = original grad",
			code: `{{FUNC}}
w = [2.0, -1.0]
gw = [0.3, 0.4]
print(ridge_gradient(w, gw, 0.0))`,
			expected: "[0.3, 0.4]\n",
		},
		{
			name: "elastic_net zero lambdas = 0.0",
			code: `{{FUNC}}
print(elastic_net([5.0, -3.0], 0.0, 0.0))`,
			expected: "0.0\n",
		},
		{
			name: "perfect predictions: precision=1.0, recall=1.0, f1=1.0",
			code: `{{FUNC}}
p, r, f1 = precision_recall_f1([1,1,0,0], [1,1,0,0])
print(round(p, 4), round(r, 4), round(f1, 4))`,
			expected: "1.0 1.0 1.0\n",
		},
		{
			name: "no true positives: precision=0, recall=0, f1=0",
			code: `{{FUNC}}
p, r, f1 = precision_recall_f1([1,1,1], [0,0,0])
print(round(p, 4), round(r, 4), round(f1, 4))`,
			expected: "0.0 0.0 0.0\n",
		},
	],
};
