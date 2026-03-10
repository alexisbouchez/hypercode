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

> **Notation note:** Here $\\sigma$ denotes the sigmoid activation function. In statistics and finance, $\\sigma$ instead represents standard deviation or volatility.

**ReLU** (Rectified Linear Unit) — the most widely used, fast and sparse:

$$\\text{ReLU}(x) = \\max(0, x)$$

**Tanh** — maps to $(-1, 1)$, zero-centered (often better than sigmoid for hidden layers):

$$\\tanh(x) = \\frac{e^x - e^{-x}}{e^x + e^{-x}}$$

### Modern Activations

**GELU** (Gaussian Error Linear Unit) — used in GPT, BERT, and most modern transformers:

$$\\text{GELU}(x) = x \\cdot \\Phi(x) \\approx 0.5 x \\left(1 + \\tanh\\left(\\sqrt{\\frac{2}{\\pi}}(x + 0.044715 x^3)\\right)\\right)$$

Unlike ReLU's hard cutoff at 0, GELU provides a smooth, probabilistic gate — small negative inputs are attenuated rather than zeroed.

**SiLU** (Sigmoid Linear Unit, a.k.a. **Swish**) — used in EfficientNet, LLaMA, and many vision models:

$$\\text{SiLU}(x) = x \\cdot \\sigma(x) = \\frac{x}{1 + e^{-x}}$$

SiLU is smooth and non-monotonic: it dips slightly below zero near $x \\approx -1.28$, which can help optimization.

### Choosing an Activation

- **Hidden layers**: ReLU (and variants) dominates modern networks — fast, avoids vanishing gradients
- **Transformer hidden layers**: GELU is the standard choice (GPT, BERT)
- **Output layer for binary classification**: Sigmoid (output is a probability)
- **Output layer for regression**: No activation (linear output)
- **Output layer for multi-class**: Softmax (next courses)

### Your Task

Implement \`sigmoid(x)\`, \`relu(x)\`, \`tanh_act(x)\`, \`gelu(x)\`, and \`silu(x)\`.`,

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

def gelu(x):
    # 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
    return 0.0

def silu(x):
    # x * sigmoid(x)
    return 0.0

print(round(sigmoid(0), 4))     # 0.5
print(round(sigmoid(2), 4))     # 0.8808
print(round(relu(-3.0), 4))     # 0.0
print(round(relu(3.0), 4))      # 3.0
print(round(tanh_act(1.0), 4))  # 0.7616
print(round(gelu(1.0), 4))      # 0.8412
print(round(silu(1.0), 4))      # 0.7311
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def relu(x):
    return max(0.0, x)

def tanh_act(x):
    return math.tanh(x)

def gelu(x):
    return 0.5 * x * (1 + math.tanh(math.sqrt(2 / math.pi) * (x + 0.044715 * x ** 3)))

def silu(x):
    return x * sigmoid(x)

print(round(sigmoid(0), 4))
print(round(sigmoid(2), 4))
print(round(relu(-3.0), 4))
print(round(relu(3.0), 4))
print(round(tanh_act(1.0), 4))
print(round(gelu(1.0), 4))
print(round(silu(1.0), 4))
`,

	tests: [
		{
			name: "sigmoid, relu, tanh, gelu, silu at standard inputs",
			expected: "0.5\n0.8808\n0.0\n3.0\n0.7616\n0.8412\n0.7311\n",
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
		{
			name: "gelu approaches relu for large positive, attenuates negatives",
			code: `{{FUNC}}
# For large positive x, GELU ≈ x (like ReLU)
print(round(gelu(3.0), 2))
# For large negative x, GELU ≈ 0 (like ReLU)
print(round(gelu(-3.0), 4))
# At 0, GELU = 0
print(round(gelu(0.0), 4))`,
			expected: "3.0\n-0.0036\n0.0\n",
		},
		{
			name: "silu is smooth and non-monotonic",
			code: `{{FUNC}}
# silu(0) = 0 * sigmoid(0) = 0
print(round(silu(0.0), 4))
# silu is negative for some negative inputs
print(silu(-2.0) < 0)
# For large x, silu ≈ x
print(round(silu(5.0), 2))`,
			expected: "0.0\nTrue\n4.97\n",
		},
	],
};
