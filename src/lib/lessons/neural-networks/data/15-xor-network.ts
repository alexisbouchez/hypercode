import type { Lesson } from "../../types";

export const xorNetwork: Lesson = {
	id: "xor-network",
	title: "Solving XOR",
	chapterId: "advanced",
	content: `## The XOR Problem

The **XOR problem** was historically significant. In 1969, Minsky & Papert proved that a single-layer perceptron *cannot* learn XOR — the function is not linearly separable:

| $x_1$ | $x_2$ | XOR |
|--------|--------|-----|
| 0 | 0 | 0 |
| 0 | 1 | 1 |
| 1 | 0 | 1 |
| 1 | 1 | 0 |

No single line can separate the 0s from the 1s. This insight contributed to the first "AI winter" — until multi-layer networks with backpropagation were rediscovered in the 1980s.

### A Two-Layer Solution

A 2-layer network with sigmoid activations can solve XOR exactly. Here is one such set of weights:

\`\`\`
Layer 1 (2→2):
  W1 = [[20, 20], [-20, -20]]
  b1 = [-10, 30]

Layer 2 (2→1):
  W2 = [20, 20]
  b2 = -30
\`\`\`

Intuition: neuron 1 learns AND, neuron 2 learns NAND. The output neuron combines them to compute OR(AND, NAND) = XOR.

### Your Task

Implement \`forward_xor(inputs, W1, b1, W2, b2)\` that:
1. Computes \`hidden[j] = sigmoid(W1[j]·inputs + b1[j])\` for each hidden neuron
2. Computes \`output = sigmoid(W2·hidden + b2)\`
3. Returns the scalar output`,

	starterCode: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def forward_xor(inputs, W1, b1, W2, b2):
    # Layer 1: hidden[j] = sigmoid(dot(W1[j], inputs) + b1[j])
    hidden = []
    # Layer 2: output = sigmoid(dot(W2, hidden) + b2)
    output = 0.0
    return output

W1 = [[20.0, 20.0], [-20.0, -20.0]]
b1 = [-10.0, 30.0]
W2 = [20.0, 20.0]
b2 = -30.0

for inp in [[0, 0], [0, 1], [1, 0], [1, 1]]:
    out = forward_xor(inp, W1, b1, W2, b2)
    print(round(out, 0))
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def forward_xor(inputs, W1, b1, W2, b2):
    hidden = [sigmoid(sum(W1[j][k] * inputs[k] for k in range(len(inputs))) + b1[j])
              for j in range(len(W1))]
    output = sigmoid(sum(W2[j] * hidden[j] for j in range(len(W2))) + b2)
    return output

W1 = [[20.0, 20.0], [-20.0, -20.0]]
b1 = [-10.0, 30.0]
W2 = [20.0, 20.0]
b2 = -30.0

for inp in [[0, 0], [0, 1], [1, 0], [1, 1]]:
    out = forward_xor(inp, W1, b1, W2, b2)
    print(round(out, 0))
`,

	tests: [
		{
			name: "XOR truth table: 0,1,1,0",
			expected: "0.0\n1.0\n1.0\n0.0\n",
		},
		{
			name: "all-zero input gives ~0",
			code: `{{FUNC}}
W1 = [[20.0, 20.0], [-20.0, -20.0]]
b1 = [-10.0, 30.0]
W2 = [20.0, 20.0]
b2 = -30.0
out = forward_xor([0, 0], W1, b1, W2, b2)
print(out < 0.01)`,
			expected: "True\n",
		},
		{
			name: "single-input gives ~1",
			code: `{{FUNC}}
W1 = [[20.0, 20.0], [-20.0, -20.0]]
b1 = [-10.0, 30.0]
W2 = [20.0, 20.0]
b2 = -30.0
out = forward_xor([1, 0], W1, b1, W2, b2)
print(out > 0.99)`,
			expected: "True\n",
		},
		{
			name: "all-one input gives ~0",
			code: `{{FUNC}}
W1 = [[20.0, 20.0], [-20.0, -20.0]]
b1 = [-10.0, 30.0]
W2 = [20.0, 20.0]
b2 = -30.0
out = forward_xor([1, 1], W1, b1, W2, b2)
print(out < 0.01)`,
			expected: "True\n",
		},
	],
};
