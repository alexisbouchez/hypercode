import type { Lesson } from "../../types";

export const decisionBoundary: Lesson = {
	id: "decision-boundary",
	title: "Decision Boundary & Metrics",
	chapterId: "supervised",
	content: `## Decision Boundary & Classification Metrics

A logistic regression model outputs a probability $\\hat{p} \\in (0,1)$. To make a hard prediction (0 or 1) we apply a **threshold** $\\tau$ (usually 0.5):

$$\\hat{y} = \\begin{cases} 1 & \\text{if } \\hat{p} \\geq \\tau \\\\ 0 & \\text{otherwise} \\end{cases}$$

### Evaluation Metrics

Once we have hard predictions we can measure quality with:

| Metric | Formula | Meaning |
|--------|---------|---------|
| **Accuracy** | $\\frac{TP + TN}{n}$ | Fraction correct |
| **Precision** | $\\frac{TP}{TP + FP}$ | Of predicted positives, how many are real? |
| **Recall** | $\\frac{TP}{TP + FN}$ | Of real positives, how many did we catch? |

where TP = true positives, TN = true negatives, FP = false positives, FN = false negatives.

**Zero-division**: if the denominator is zero, return 0.0.

### Your Task

Implement:
- \`classify(x, w, b, threshold=0.5)\` → 0 or 1
- \`accuracy(y_pred, y_true)\` → fraction of correct predictions
- \`precision(y_pred, y_true)\` → TP / (TP + FP), 0.0 on zero-division
- \`recall(y_pred, y_true)\` → TP / (TP + FN), 0.0 on zero-division`,

	starterCode: `import math

def sigmoid(z):
    return 1 / (1 + math.exp(-z))

def logistic_predict(x, w, b):
    return sigmoid(w * x + b)

def classify(x, w, b, threshold=0.5):
    # Return 1 if logistic_predict >= threshold else 0
    return 0

def accuracy(y_pred, y_true):
    # Fraction of matching predictions
    return 0.0

def precision(y_pred, y_true):
    # TP / (TP + FP), return 0.0 on zero-division
    return 0.0

def recall(y_pred, y_true):
    # TP / (TP + FN), return 0.0 on zero-division
    return 0.0

print(classify(2.0, 1.0, -1.0))    # 1
print(classify(-2.0, 1.0, -1.0))   # 0
y_pred = [1, 1, 0, 0]
y_true = [1, 0, 0, 1]
print(round(accuracy(y_pred, y_true), 4))    # 0.5
print(round(precision(y_pred, y_true), 4))   # 0.5
print(round(recall(y_pred, y_true), 4))      # 0.5
`,

	solution: `import math

def sigmoid(z):
    return 1 / (1 + math.exp(-z))

def logistic_predict(x, w, b):
    return sigmoid(w * x + b)

def classify(x, w, b, threshold=0.5):
    return 1 if logistic_predict(x, w, b) >= threshold else 0

def accuracy(y_pred, y_true):
    return sum(p == t for p, t in zip(y_pred, y_true)) / len(y_true)

def precision(y_pred, y_true):
    tp = sum(p == 1 and t == 1 for p, t in zip(y_pred, y_true))
    fp = sum(p == 1 and t == 0 for p, t in zip(y_pred, y_true))
    return tp / (tp + fp) if (tp + fp) > 0 else 0.0

def recall(y_pred, y_true):
    tp = sum(p == 1 and t == 1 for p, t in zip(y_pred, y_true))
    fn = sum(p == 0 and t == 1 for p, t in zip(y_pred, y_true))
    return tp / (tp + fn) if (tp + fn) > 0 else 0.0

print(classify(2.0, 1.0, -1.0))
print(classify(-2.0, 1.0, -1.0))
y_pred = [1, 1, 0, 0]
y_true = [1, 0, 0, 1]
print(round(accuracy(y_pred, y_true), 4))
print(round(precision(y_pred, y_true), 4))
print(round(recall(y_pred, y_true), 4))
`,

	tests: [
		{
			name: "classify and metrics: accuracy=0.5, precision=0.5, recall=0.5",
			expected: "1\n0\n0.5\n0.5\n0.5\n",
		},
		{
			name: "all correct: accuracy=1.0, precision=1.0, recall=1.0",
			code: `{{FUNC}}
y2p = [1, 1, 0, 0]
y2t = [1, 1, 0, 0]
print(round(accuracy(y2p, y2t), 4))
print(round(precision(y2p, y2t), 4))
print(round(recall(y2p, y2t), 4))`,
			expected: "1.0\n1.0\n1.0\n",
		},
		{
			name: "precision zero-division returns 0.0",
			code: `{{FUNC}}
print(precision([0, 0], [1, 1]))`,
			expected: "0.0\n",
		},
		{
			name: "recall zero-division returns 0.0",
			code: `{{FUNC}}
print(recall([0, 0], [0, 0]))`,
			expected: "0.0\n",
		},
		{
			name: "classify uses threshold correctly",
			code: `{{FUNC}}
print(classify(0.0, 0.0, 0.0))`,
			expected: "1\n",
		},
	],
};
