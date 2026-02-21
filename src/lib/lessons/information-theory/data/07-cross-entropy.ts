import type { Lesson } from "../../types";

export const crossEntropy: Lesson = {
	id: "cross-entropy",
	title: "Cross-Entropy",
	chapterId: "divergence",
	content: `## Cross-Entropy

**Cross-entropy** $H(P, Q)$ measures the average number of bits needed to encode events from distribution $P$ using a code optimized for distribution $Q$:

$$H(P, Q) = -\\sum_i p_i \\log_2 q_i$$

### Relationship to KL Divergence

$$H(P, Q) = H(P) + D_{KL}(P \\| Q)$$

This decomposition shows:
- $H(P)$ — the irreducible entropy of $P$ (minimum bits needed)
- $D_{KL}(P \\| Q)$ — extra bits wasted by using the wrong model $Q$

Since $D_{KL} \\geq 0$, we have $H(P, Q) \\geq H(P)$ always.

### In Machine Learning

Cross-entropy is the standard **loss function for classification**. With true labels $P$ (one-hot vectors) and model predictions $Q$ (softmax outputs):

$$\\mathcal{L} = -\\sum_i y_i \\log_2 \\hat{y}_i$$

When $P$ is one-hot (true class probability = 1, all others = 0), this simplifies to just $-\\log_2(\\hat{y}_{\\text{true}})$.

\`\`\`python
import math

def cross_entropy(p, q):
    epsilon = 1e-15
    return -sum(p[i] * math.log2(q[i] + epsilon)
                for i in range(len(p)) if p[i] > 0)

p = [0.8, 0.2]
q = [0.6, 0.4]
print(round(cross_entropy(p, q), 4))  # 0.854
\`\`\`

### Your Task

Implement:
- \`cross_entropy(p, q)\` — $H(P,Q) = -\\sum p_i \\log_2(q_i + \\varepsilon)$ for $p_i > 0$
- \`cross_entropy_loss(y_true, y_pred)\` — same function, ML naming convention`,

	starterCode: `import math

def cross_entropy(p, q):
    # H(P,Q) = -sum(p[i] * log2(q[i] + 1e-15) for p[i] > 0)
    pass

def cross_entropy_loss(y_true, y_pred):
    # Same as cross_entropy, ML convention name
    pass

p = [0.8, 0.2]
q = [0.6, 0.4]
print(round(cross_entropy(p, q), 4))
print(round(cross_entropy_loss([1.0, 0.0], [0.9, 0.1]), 4))
`,

	solution: `import math

def cross_entropy(p, q):
    epsilon = 1e-15
    return -sum(p[i] * math.log2(q[i] + epsilon)
                for i in range(len(p)) if p[i] > 0)

def cross_entropy_loss(y_true, y_pred):
    epsilon = 1e-15
    return -sum(y_true[i] * math.log2(y_pred[i] + epsilon)
                for i in range(len(y_true)) if y_true[i] > 0)

p = [0.8, 0.2]
q = [0.6, 0.4]
print(round(cross_entropy(p, q), 4))
print(round(cross_entropy_loss([1.0, 0.0], [0.9, 0.1]), 4))
`,

	tests: [
		{
			name: "cross_entropy([0.8,0.2],[0.6,0.4])=0.854, loss([1,0],[0.9,0.1])=0.152",
			expected: "0.854\n0.152\n",
		},
		{
			name: "cross_entropy([0.5,0.5],[0.5,0.5]) = 1.0",
			code: `{{FUNC}}
print(round(cross_entropy([0.5, 0.5], [0.5, 0.5]), 4))`,
			expected: "1.0\n",
		},
		{
			name: "H(p,q) >= H(p): cross entropy >= shannon entropy",
			code: `{{FUNC}}
import math
p = [0.6, 0.4]
q = [0.3, 0.7]
hp = sum(-x * math.log2(x) for x in p if x > 0)
hpq = cross_entropy(p, q)
print(round(hpq, 4) >= round(hp, 4))`,
			expected: "True\n",
		},
		{
			name: "cross_entropy_loss([1.0,0.0],[0.9,0.1]) = cross_entropy([1.0,0.0],[0.9,0.1])",
			code: `{{FUNC}}
a = cross_entropy([1.0, 0.0], [0.9, 0.1])
b = cross_entropy_loss([1.0, 0.0], [0.9, 0.1])
print(round(a, 10) == round(b, 10))`,
			expected: "True\n",
		},
		{
			name: "round(cross_entropy([0.5,0.5],[0.25,0.75]), 4) = 1.2075",
			code: `{{FUNC}}
print(round(cross_entropy([0.5, 0.5], [0.25, 0.75]), 4))`,
			expected: "1.2075\n",
		},
	],
};
