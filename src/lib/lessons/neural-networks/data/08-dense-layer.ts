import type { Lesson } from "../../types";

export const denseLayer: Lesson = {
	id: "dense-layer",
	title: "The Dense Layer",
	chapterId: "backpropagation",
	content: `## Vectorizing to Multiple Neurons

A single neuron maps $\\mathbb{R}^n \\to \\mathbb{R}$. A **dense layer** (fully connected layer) stacks $m$ neurons in parallel, mapping $\\mathbb{R}^n \\to \\mathbb{R}^m$:

$$\\mathbf{z} = W\\mathbf{x} + \\mathbf{b}$$

- $W \\in \\mathbb{R}^{m \\times n}$ — weight matrix, row $j$ are the weights of neuron $j$
- $\\mathbf{b} \\in \\mathbb{R}^m$ — bias vector
- $\\mathbf{x} \\in \\mathbb{R}^n$ — input vector
- $\\mathbf{z} \\in \\mathbb{R}^m$ — pre-activation outputs

Each output $z_j = \\sum_{k=1}^n W_{jk} x_k + b_j$ is an independent neuron computation.

### DenseLayer Class

We represent a layer as an object with:
- \`weights\`: list of $m$ rows, each of length $n$
- \`biases\`: list of $m$ scalars
- \`forward(inputs)\`: computes $W\\mathbf{x} + \\mathbf{b}$

We initialize with small Gaussian weights (standard deviation 0.1) and zero biases. Large initial weights cause saturated activations that kill gradients.

### Your Task

Implement the \`DenseLayer\` class with:
- \`__init__(self, in_features, out_features)\` — initialize weights with \`random.gauss(0, 0.1)\` (set \`random.seed(42)\` before the loop) and zero biases
- \`forward(self, inputs)\` — compute and return the output vector`,

	starterCode: `import random

class DenseLayer:
    def __init__(self, in_features, out_features):
        random.seed(42)
        # weights: out_features rows, each of length in_features
        # biases: out_features zeros
        self.weights = []
        self.biases = []

    def forward(self, inputs):
        # For each neuron j: sum(weights[j][k] * inputs[k]) + biases[j]
        return []

layer = DenseLayer(2, 3)
layer.weights = [[1.0, 0.0], [0.0, 1.0], [0.5, 0.5]]
layer.biases = [0.1, 0.2, 0.3]
out = layer.forward([2.0, 3.0])
for v in out:
    print(round(v, 4))
`,

	solution: `import random

class DenseLayer:
    def __init__(self, in_features, out_features):
        random.seed(42)
        self.weights = [[random.gauss(0, 0.1) for _ in range(in_features)] for _ in range(out_features)]
        self.biases = [0.0] * out_features

    def forward(self, inputs):
        return [sum(w * x for w, x in zip(row, inputs)) + b
                for row, b in zip(self.weights, self.biases)]

layer = DenseLayer(2, 3)
layer.weights = [[1.0, 0.0], [0.0, 1.0], [0.5, 0.5]]
layer.biases = [0.1, 0.2, 0.3]
out = layer.forward([2.0, 3.0])
for v in out:
    print(round(v, 4))
`,

	tests: [
		{
			name: "dense layer forward pass with explicit weights",
			expected: "2.1\n3.2\n2.8\n",
		},
		{
			name: "identity layer",
			code: `{{FUNC}}
layer = DenseLayer(2, 2)
layer.weights = [[1.0, 0.0], [0.0, 1.0]]
layer.biases = [0.5, -0.5]
out = layer.forward([3.0, 4.0])
for v in out:
    print(round(v, 4))`,
			expected: "3.5\n3.5\n",
		},
		{
			name: "output size matches out_features",
			code: `{{FUNC}}
layer = DenseLayer(4, 8)
out = layer.forward([0.0]*4)
print(len(out))`,
			expected: "8\n",
		},
	],
};
