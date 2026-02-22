import type { Lesson } from "../../types";

export const activations: Lesson = {
	id: "activations",
	title: "Activation Functions",
	chapterId: "foundations",
	content: `## Why Activation Functions?

Without an activation function, stacking multiple layers collapses to a single linear transformation:

$$W_2(W_1 x + b_1) + b_2 = (W_2 W_1)x + (W_2 b_1 + b_2)$$

This is just another linear function. A deep network of linear layers is no more powerful than a single layer. **Non-linearity** is what lets neural networks approximate any function.

### The Three Classics

**Sigmoid** — maps any input to $(0, 1)$, useful for probabilities:

$$\\sigma(x) = \\frac{1}{1 + e^{-x}}$$

**ReLU** (Rectified Linear Unit) — the most widely used, fast and sparse:

$$\\text{ReLU}(x) = \\max(0, x)$$

**Tanh** — maps to $(-1, 1)$, zero-centered (often better than sigmoid for hidden layers):

$$\\tanh(x) = \\frac{e^x - e^{-x}}{e^x + e^{-x}}$$

### Choosing an Activation

- **Hidden layers**: ReLU (and variants) dominates modern networks — fast, avoids vanishing gradients
- **Output layer for binary classification**: Sigmoid (output is a probability)
- **Output layer for regression**: No activation (linear output)
- **Output layer for multi-class**: Softmax (next courses)

### Your Task

Implement \`sigmoid(x)\`, \`relu(x)\`, and \`tanh_act(x)\`.`,

	starterCode: `import math

def sigmoid(x):
    # 1 / (1 + e^(-x))
    return 0.0

def relu(x):
    # max(0, x)
    return 0.0

def tanh_act(x):
    # math.tanh(x)
    return 0.0

print(round(sigmoid(0), 4))     # 0.5
print(round(sigmoid(2), 4))     # 0.8808
print(round(relu(-3.0), 4))     # 0.0
print(round(relu(3.0), 4))      # 3.0
print(round(tanh_act(1.0), 4))  # 0.7616
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def relu(x):
    return max(0.0, x)

def tanh_act(x):
    return math.tanh(x)

print(round(sigmoid(0), 4))
print(round(sigmoid(2), 4))
print(round(relu(-3.0), 4))
print(round(relu(3.0), 4))
print(round(tanh_act(1.0), 4))
`,

	tests: [
		{
			name: "sigmoid, relu, tanh at standard inputs",
			expected: "0.5\n0.8808\n0.0\n3.0\n0.7616\n",
		},
		{
			name: "sigmoid is symmetric around 0",
			code: `{{FUNC}}
print(round(sigmoid(0), 4))
print(round(sigmoid(-2), 4))
print(round(1 - sigmoid(2), 4))`,
			expected: "0.5\n0.1192\n0.1192\n",
		},
		{
			name: "relu zeros out negatives",
			code: `{{FUNC}}
for x in [-10.0, -0.001, 0.0, 0.001, 10.0]:
    print(relu(x))`,
			expected: "0.0\n0.0\n0.0\n0.001\n10.0\n",
		},
	],
};
