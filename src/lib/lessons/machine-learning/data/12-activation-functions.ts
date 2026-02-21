import type { Lesson } from "../../types";

export const activationFunctions: Lesson = {
	id: "activation-functions",
	title: "Activation Functions",
	chapterId: "neural-networks",
	content: `## Activation Functions

Activation functions introduce **non-linearity** into neural networks. Without them, stacking linear layers is equivalent to a single linear layer — the network could not learn complex patterns.

### ReLU

**Rectified Linear Unit** — the most widely used activation in modern deep learning:

$$\\text{ReLU}(x) = \\max(0, x)$$

Cheap to compute and avoids the vanishing gradient problem.

### Leaky ReLU

Fixes the "dying ReLU" problem by allowing a small gradient for negative inputs:

$$\\text{LeakyReLU}(x) = \\begin{cases} x & x > 0 \\\\ \\alpha x & x \\leq 0 \\end{cases}$$

where $\\alpha$ is a small constant (default $0.01$).

### Tanh

The hyperbolic tangent squashes inputs to $(-1, 1)$:

$$\\tanh(x) = \\frac{e^{2x} - 1}{e^{2x} + 1}$$

### Softmax

Used in the output layer for multi-class classification. Converts a vector of logits into a probability distribution:

$$\\text{softmax}(x)_i = \\frac{e^{x_i}}{\\sum_j e^{x_j}}$$

All outputs are in $(0,1)$ and sum to 1.

### Your Task

Implement:
- \`relu(x)\` → $\\max(0, x)$
- \`leaky_relu(x, alpha=0.01)\`
- \`tanh_activation(x)\` → computed via $(e^{2x}-1)/(e^{2x}+1)$
- \`softmax(x)\` → list of probabilities`,

	starterCode: `import math

def relu(x):
    return 0.0

def leaky_relu(x, alpha=0.01):
    return 0.0

def tanh_activation(x):
    # (exp(2x) - 1) / (exp(2x) + 1)
    return 0.0

def softmax(x):
    # exp(xi) / sum(exp(xj)) for each i
    return []

print(relu(-5))                                      # 0
print(relu(3))                                       # 3
print(leaky_relu(-4, 0.01))                         # -0.04
print(round(tanh_activation(0), 4))                  # 0.0
print(round(tanh_activation(1), 4))                  # 0.7616
print([round(v, 4) for v in softmax([1, 1, 1])])     # [0.3333, 0.3333, 0.3333]
`,

	solution: `import math

def relu(x):
    return max(0, x)

def leaky_relu(x, alpha=0.01):
    return x if x > 0 else alpha * x

def tanh_activation(x):
    return (math.exp(2 * x) - 1) / (math.exp(2 * x) + 1)

def softmax(x):
    total = sum(math.exp(xi) for xi in x)
    return [math.exp(xi) / total for xi in x]

print(relu(-5))
print(relu(3))
print(leaky_relu(-4, 0.01))
print(round(tanh_activation(0), 4))
print(round(tanh_activation(1), 4))
print([round(v, 4) for v in softmax([1, 1, 1])])
`,

	tests: [
		{
			name: "relu, leaky_relu, tanh, softmax basics",
			expected: "0\n3\n-0.04\n0.0\n0.7616\n[0.3333, 0.3333, 0.3333]\n",
		},
		{
			name: "relu(0) = 0",
			code: `{{FUNC}}
print(relu(0))`,
			expected: "0\n",
		},
		{
			name: "leaky_relu with alpha=0.1 and negative input",
			code: `{{FUNC}}
print(round(leaky_relu(-4, 0.1), 4))`,
			expected: "-0.4\n",
		},
		{
			name: "softmax([0,0,0]) = [0.3333, 0.3333, 0.3333]",
			code: `{{FUNC}}
print([round(v, 4) for v in softmax([0, 0, 0])])`,
			expected: "[0.3333, 0.3333, 0.3333]\n",
		},
		{
			name: "tanh_activation of large positive input ≈ 1.0",
			code: `{{FUNC}}
print(round(tanh_activation(10), 4))`,
			expected: "1.0\n",
		},
	],
};
