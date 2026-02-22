import type { Lesson } from "../../types";

export const twoLayerBackprop: Lesson = {
	id: "two-layer-backprop",
	title: "Multi-Layer Backpropagation",
	chapterId: "backpropagation",
	content: `## Backprop Through Two Layers

Now extend backpropagation to a two-layer network. The architecture:

$$z_1 = w_1 x + b_1 \\qquad a_1 = \\text{ReLU}(z_1)$$

$$z_2 = w_2 a_1 + b_2 \\qquad a_2 = \\sigma(z_2)$$

$$\\mathcal{L} = (a_2 - y)^2$$

### Forward Pass

Compute $z_1 \\to a_1 \\to z_2 \\to a_2 \\to \\mathcal{L}$ in order.

### Backward Pass

Work backwards using the chain rule:

$$\\frac{\\partial \\mathcal{L}}{\\partial w_2} = \\frac{\\partial \\mathcal{L}}{\\partial a_2} \\cdot \\frac{\\partial a_2}{\\partial z_2} \\cdot a_1$$

$$\\frac{\\partial \\mathcal{L}}{\\partial b_2} = \\frac{\\partial \\mathcal{L}}{\\partial a_2} \\cdot \\frac{\\partial a_2}{\\partial z_2}$$

To reach layer 1, we propagate the error signal *through* $w_2$:

$$\\frac{\\partial \\mathcal{L}}{\\partial a_1} = \\frac{\\partial \\mathcal{L}}{\\partial z_2} \\cdot w_2$$

$$\\frac{\\partial \\mathcal{L}}{\\partial w_1} = \\frac{\\partial \\mathcal{L}}{\\partial a_1} \\cdot \\text{ReLU}'(z_1) \\cdot x$$

$$\\frac{\\partial \\mathcal{L}}{\\partial b_1} = \\frac{\\partial \\mathcal{L}}{\\partial a_1} \\cdot \\text{ReLU}'(z_1)$$

This is the **backpropagation algorithm**: forward to compute activations, backward to propagate error signals.

### Your Task

Implement \`two_layer_backward(x, w1, b1, w2, b2, y)\` returning \`(dw1, db1, dw2, db2)\`.`,

	starterCode: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def relu(x):
    return max(0.0, x)

def relu_grad(x):
    return 1.0 if x > 0 else 0.0

def two_layer_backward(x, w1, b1, w2, b2, y):
    # Forward
    z1 = w1 * x + b1
    a1 = relu(z1)
    z2 = w2 * a1 + b2
    a2 = sigmoid(z2)
    # Backward — fill in the gradients
    dw2, db2, dw1, db1 = 0.0, 0.0, 0.0, 0.0
    return dw1, db1, dw2, db2

dw1, db1, dw2, db2 = two_layer_backward(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
print(round(dw1, 4), round(db1, 4), round(dw2, 4), round(db2, 4))
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def relu(x):
    return max(0.0, x)

def relu_grad(x):
    return 1.0 if x > 0 else 0.0

def two_layer_backward(x, w1, b1, w2, b2, y):
    z1 = w1 * x + b1
    a1 = relu(z1)
    z2 = w2 * a1 + b2
    a2 = sigmoid(z2)
    dL_da2 = 2 * (a2 - y)
    da2_dz2 = a2 * (1 - a2)
    dw2 = dL_da2 * da2_dz2 * a1
    db2 = dL_da2 * da2_dz2
    da1 = dL_da2 * da2_dz2 * w2
    dz1 = da1 * relu_grad(z1)
    dw1 = dz1 * x
    db1 = dz1
    return dw1, db1, dw2, db2

dw1, db1, dw2, db2 = two_layer_backward(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
print(round(dw1, 4), round(db1, 4), round(dw2, 4), round(db2, 4))
`,

	tests: [
		{
			name: "dead ReLU blocks gradient to layer 1",
			expected: "0.0 0.0 0.0 0.25\n",
		},
		{
			name: "active ReLU propagates gradient",
			code: `{{FUNC}}
dw1, db1, dw2, db2 = two_layer_backward(1.0, 1.0, 0.0, 1.0, 0.0, 1.0)
print(round(dw1, 4), round(db1, 4), round(dw2, 4), round(db2, 4))`,
			expected: "-0.1058 -0.1058 -0.1058 -0.1058\n",
		},
		{
			name: "dw2 scales with a1, dw1 scales with x",
			code: `{{FUNC}}
dw1, db1, dw2, db2 = two_layer_backward(2.0, 1.0, 0.0, 1.0, 0.0, 0.0)
print(round(dw1, 4), round(db1, 4), round(dw2, 4), round(db2, 4))`,
			expected: "0.3699 0.185 0.3699 0.185\n",
		},
	],
};
