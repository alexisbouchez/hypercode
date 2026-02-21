import type { Lesson } from "../../types";

export const forwardPass: Lesson = {
	id: "forward-pass",
	title: "Neural Network Forward Pass",
	chapterId: "neural-networks",
	content: `## Neural Network Forward Pass

A neural network layer takes an input vector $\\mathbf{x}$ and applies a weight matrix $W$ and bias vector $\\mathbf{b}$:

$$\\mathbf{z} = W\\mathbf{x} + \\mathbf{b}$$

Each row $W_i$ contains the weights for one neuron:

$$z_i = W_i \\cdot \\mathbf{x} + b_i = \\sum_j W_{ij} x_j + b_i$$

Then an **activation function** $\\sigma$ is applied element-wise:

$$\\mathbf{a} = \\sigma(\\mathbf{z})$$

### Two-Layer Network

A network with two layers computes:

$$\\mathbf{a}^{(1)} = \\text{ReLU}(W^{(1)}\\mathbf{x} + \\mathbf{b}^{(1)})$$
$$\\hat{y} = W^{(2)}\\mathbf{a}^{(1)} + b^{(2)}$$

### Your Task

Implement:
- \`layer_output(x, W, b)\` → list: $[W_i \\cdot x + b_i]$ for each neuron row $W_i$
- \`relu_layer(x, W, b)\` → apply ReLU to each element of \`layer_output(x, W, b)\``,

	starterCode: `def dot(x, w):
    return sum(xi * wi for xi, wi in zip(x, w))

def relu(x):
    return max(0, x)

def layer_output(x, W, b):
    # For each row W[i] compute dot(x, W[i]) + b[i]
    return []

def relu_layer(x, W, b):
    # Apply relu to each element of layer_output
    return []

x = [1.0, 2.0]
W = [[1.0, 0.0], [0.0, 1.0]]
b = [0.5, 0.5]
print(layer_output(x, W, b))    # [1.5, 2.5]

W2 = [[1.0, -1.0], [-1.0, 1.0]]
b2 = [0.0, 0.0]
print(relu_layer(x, W2, b2))    # [0, 1.0]
`,

	solution: `def dot(x, w):
    return sum(xi * wi for xi, wi in zip(x, w))

def relu(x):
    return max(0, x)

def layer_output(x, W, b):
    return [dot(x, W[i]) + b[i] for i in range(len(W))]

def relu_layer(x, W, b):
    return [relu(v) for v in layer_output(x, W, b)]

x = [1.0, 2.0]
W = [[1.0, 0.0], [0.0, 1.0]]
b = [0.5, 0.5]
print(layer_output(x, W, b))

W2 = [[1.0, -1.0], [-1.0, 1.0]]
b2 = [0.0, 0.0]
print(relu_layer(x, W2, b2))
`,

	tests: [
		{
			name: "layer_output=[1.5,2.5], relu_layer clamps negative to 0",
			expected: "[1.5, 2.5]\n[0, 1.0]\n",
		},
		{
			name: "layer_output single neuron",
			code: `{{FUNC}}
x = [2.0, 3.0]
W = [[1.0, 1.0]]
b = [-1.0]
print(layer_output(x, W, b))`,
			expected: "[4.0]\n",
		},
		{
			name: "relu_layer all positive outputs unchanged",
			code: `{{FUNC}}
x = [1.0, 1.0]
W = [[2.0, 1.0], [1.0, 2.0]]
b = [0.0, 0.0]
print(relu_layer(x, W, b))`,
			expected: "[3.0, 3.0]\n",
		},
		{
			name: "relu_layer clamps all negative outputs to 0",
			code: `{{FUNC}}
x = [1.0, 1.0]
W = [[-1.0, -1.0]]
b = [0.0]
print(relu_layer(x, W, b))`,
			expected: "[0]\n",
		},
	],
};
