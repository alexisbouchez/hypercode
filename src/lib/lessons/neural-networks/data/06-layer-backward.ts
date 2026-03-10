import type { Lesson } from "../../types";

export const layerBackward: Lesson = {
	id: "layer-backward",
	title: "Layer Backpropagation",
	chapterId: "gradients",
	content: `## Gradients Through One Layer

Consider a single neuron with sigmoid activation ($\\sigma$ = sigmoid function, not standard deviation) and MSE loss:

$$z = \\mathbf{w} \\cdot \\mathbf{x} + b \\qquad a = \\sigma(z) \\qquad \\mathcal{L} = (a - y)^2$$

To update $\\mathbf{w}$ and $b$, we need $\\frac{\\partial \\mathcal{L}}{\\partial \\mathbf{w}}$ and $\\frac{\\partial \\mathcal{L}}{\\partial b}$.

### Applying the Chain Rule

$$\\frac{\\partial \\mathcal{L}}{\\partial w_i} = \\frac{\\partial \\mathcal{L}}{\\partial a} \\cdot \\frac{\\partial a}{\\partial z} \\cdot \\frac{\\partial z}{\\partial w_i}$$

Each factor:

$$\\frac{\\partial \\mathcal{L}}{\\partial a} = 2(a - y)$$

$$\\frac{\\partial a}{\\partial z} = \\sigma'(z) = a(1-a)$$

$$\\frac{\\partial z}{\\partial w_i} = x_i \\qquad \\frac{\\partial z}{\\partial b} = 1$$

Define the **error signal** $\\delta = \\frac{\\partial \\mathcal{L}}{\\partial a} \\cdot \\frac{\\partial a}{\\partial z} = 2(a-y) \\cdot a(1-a)$. Then:

$$\\frac{\\partial \\mathcal{L}}{\\partial w_i} = \\delta \\cdot x_i \\qquad \\frac{\\partial \\mathcal{L}}{\\partial b} = \\delta$$

### Your Task

Implement \`layer_backward(inputs, weights, bias, target)\` that:
1. Performs the forward pass to compute $a$
2. Computes $\\delta = 2(a - y) \\cdot a(1-a)$
3. Returns \`(dw, db)\` where \`dw[i] = delta * inputs[i]\``,

	starterCode: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def layer_backward(inputs, weights, bias, target):
    # Forward pass
    z = sum(w * x for w, x in zip(weights, inputs)) + bias
    a = sigmoid(z)
    # Backward pass: dL/da = 2*(a-target), da/dz = a*(1-a)
    # delta = dL/da * da/dz
    # dw[i] = delta * inputs[i], db = delta
    return [], 0.0

dw, db = layer_backward([1.0], [0.0], 0.0, 1.0)
print(round(dw[0], 4))   # -0.25
print(round(db, 4))       # -0.25
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def layer_backward(inputs, weights, bias, target):
    z = sum(w * x for w, x in zip(weights, inputs)) + bias
    a = sigmoid(z)
    dL_da = 2 * (a - target)
    da_dz = a * (1 - a)
    delta = dL_da * da_dz
    dw = [delta * x for x in inputs]
    db = delta
    return dw, db

dw, db = layer_backward([1.0], [0.0], 0.0, 1.0)
print(round(dw[0], 4))
print(round(db, 4))
`,

	tests: [
		{
			name: "gradient at sigmoid midpoint (a=0.5, target=1)",
			expected: "-0.25\n-0.25\n",
		},
		{
			name: "zero input gives zero weight gradient",
			code: `{{FUNC}}
dw, db = layer_backward([0.0], [1.0], 0.0, 0.0)
print(round(dw[0], 4))
print(round(db, 4))`,
			expected: "0.0\n0.25\n",
		},
		{
			name: "gradient scales with input magnitude",
			code: `{{FUNC}}
dw1, db1 = layer_backward([1.0, 2.0], [0.5, -0.5], 0.0, 0.0)
print([round(v, 4) for v in dw1])
print(round(db1, 4))`,
			expected: "[0.1774, 0.3549]\n0.1774\n",
		},
		{
			name: "numerical gradient check matches analytical gradients",
			code: `{{FUNC}}
# Numerical gradient check: verify analytical gradients via finite differences
inputs = [1.5, -0.7]
weights = [0.3, -0.8]
bias = 0.2
target = 0.9
h = 1e-5

def loss_fn(inp, w, b, t):
    z = sum(wi * xi for wi, xi in zip(w, inp)) + b
    a = sigmoid(z)
    return (a - t) ** 2

dw_anal, db_anal = layer_backward(inputs, weights, bias, target)

# Numerical gradient for each weight
ok = True
for i in range(len(weights)):
    w_plus = list(weights)
    w_minus = list(weights)
    w_plus[i] += h
    w_minus[i] -= h
    dw_num = (loss_fn(inputs, w_plus, bias, target) - loss_fn(inputs, w_minus, bias, target)) / (2 * h)
    if abs(dw_anal[i] - dw_num) > 1e-4:
        ok = False

# Numerical gradient for bias
loss_plus = loss_fn(inputs, weights, bias + h, target)
loss_minus = loss_fn(inputs, weights, bias - h, target)
db_num = (loss_plus - loss_minus) / (2 * h)
if abs(db_anal - db_num) > 1e-4:
    ok = False

print(ok)`,
			expected: "True\n",
		},
	],
};
