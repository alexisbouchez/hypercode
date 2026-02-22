import type { Lesson } from "../../types";

export const networkForward: Lesson = {
	id: "network-forward",
	title: "Network Forward Pass",
	chapterId: "backpropagation",
	content: `## Composing Layers

A neural network is a sequence of layers. The forward pass is just function composition:

$$\\mathbf{a}^{(0)} = \\mathbf{x}$$

$$\\mathbf{a}^{(l)} = f^{(l)}(\\mathbf{a}^{(l-1)}) \\quad \\text{for } l = 1, 2, \\ldots, L$$

Where each $f^{(l)}$ is either a \`DenseLayer\` or an \`ActivationLayer\`.

### Activation Layers

An \`ActivationLayer\` simply applies a scalar function element-wise to its input:

$$\\mathbf{a} = [f(z_1), f(z_2), \\ldots, f(z_m)]$$

### Network Architecture

A typical two-hidden-layer network looks like:

\`\`\`
Input → Dense(n, 64) → ReLU → Dense(64, 32) → ReLU → Dense(32, 1) → Sigmoid
\`\`\`

Each \`Dense\` layer mixes features. Each \`Activation\` layer introduces non-linearity.

### Your Task

Implement:
- \`ActivationLayer(activation)\` with a \`forward(inputs)\` method that applies \`activation\` element-wise
- \`Network(layers)\` with a \`forward(inputs)\` method that passes data through each layer in sequence`,

	starterCode: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def relu(x):
    return max(0.0, x)

class ActivationLayer:
    def __init__(self, activation):
        self.activation = activation

    def forward(self, inputs):
        # Apply self.activation to each element
        return []

class Network:
    def __init__(self, layers):
        self.layers = layers

    def forward(self, inputs):
        # Pass inputs through each layer in sequence
        return inputs

net = Network([ActivationLayer(relu), ActivationLayer(sigmoid)])
out = net.forward([-1.0, 0.0, 2.0])
for v in out:
    print(round(v, 4))
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def relu(x):
    return max(0.0, x)

class ActivationLayer:
    def __init__(self, activation):
        self.activation = activation

    def forward(self, inputs):
        return [self.activation(x) for x in inputs]

class Network:
    def __init__(self, layers):
        self.layers = layers

    def forward(self, inputs):
        x = inputs
        for layer in self.layers:
            x = layer.forward(x)
        return x

net = Network([ActivationLayer(relu), ActivationLayer(sigmoid)])
out = net.forward([-1.0, 0.0, 2.0])
for v in out:
    print(round(v, 4))
`,

	tests: [
		{
			name: "relu then sigmoid pipeline",
			expected: "0.5\n0.5\n0.8808\n",
		},
		{
			name: "single activation layer",
			code: `{{FUNC}}
layer = ActivationLayer(relu)
print(layer.forward([-2.0, -1.0, 0.0, 1.0, 2.0]))`,
			expected: "[0.0, 0.0, 0.0, 1.0, 2.0]\n",
		},
		{
			name: "identity network with no layers",
			code: `{{FUNC}}
net = Network([])
print(net.forward([1.0, 2.0, 3.0]))`,
			expected: "[1.0, 2.0, 3.0]\n",
		},
	],
};
