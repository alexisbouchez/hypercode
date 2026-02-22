import type { Lesson } from "../../types";

export const gradientStep: Lesson = {
	id: "gradient-step",
	title: "Gradient Descent Step",
	chapterId: "training",
	content: `## Updating the Weights

Once we have gradients $\\frac{\\partial \\mathcal{L}}{\\partial W}$ and $\\frac{\\partial \\mathcal{L}}{\\partial \\mathbf{b}}$, we update the parameters by taking a step **opposite** to the gradient:

$$W \\leftarrow W - \\eta \\cdot \\frac{\\partial \\mathcal{L}}{\\partial W}$$

$$\\mathbf{b} \\leftarrow \\mathbf{b} - \\eta \\cdot \\frac{\\partial \\mathcal{L}}{\\partial \\mathbf{b}}$$

Where $\\eta$ (eta) is the **learning rate** — how large a step to take.

### Choosing the Learning Rate

- **Too large**: loss oscillates or diverges (overshoot the minimum)
- **Too small**: training is unnecessarily slow
- **Just right**: loss decreases smoothly

Typical starting values: $0.1$ for small networks, $10^{-3}$ for deep networks with Adam.

### Gradient Descent Update

For a layer with weight matrix $W$ (shape $m \\times n$) and bias vector $\\mathbf{b}$ (length $m$):

$$W_{jk} \\leftarrow W_{jk} - \\eta \\cdot \\frac{\\partial \\mathcal{L}}{\\partial W_{jk}}$$

$$b_j \\leftarrow b_j - \\eta \\cdot \\frac{\\partial \\mathcal{L}}{\\partial b_j}$$

### Your Task

Implement \`gradient_step(weights, biases, dw, db, lr)\` that returns updated \`(new_weights, new_biases)\`.`,

	starterCode: `def gradient_step(weights, biases, dw, db, lr):
    # new_weights[j][k] = weights[j][k] - lr * dw[j][k]
    # new_biases[j] = biases[j] - lr * db[j]
    new_weights = weights
    new_biases = biases
    return new_weights, new_biases

weights = [[1.0, 2.0], [3.0, 4.0]]
biases = [0.5, -0.5]
dw = [[0.1, 0.2], [0.3, 0.4]]
db = [0.05, -0.1]
lr = 0.1

nw, nb = gradient_step(weights, biases, dw, db, lr)
for row in nw:
    print([round(v, 4) for v in row])
print([round(v, 4) for v in nb])
`,

	solution: `def gradient_step(weights, biases, dw, db, lr):
    new_weights = [[w - lr * g for w, g in zip(row_w, row_dw)]
                   for row_w, row_dw in zip(weights, dw)]
    new_biases = [b - lr * g for b, g in zip(biases, db)]
    return new_weights, new_biases

weights = [[1.0, 2.0], [3.0, 4.0]]
biases = [0.5, -0.5]
dw = [[0.1, 0.2], [0.3, 0.4]]
db = [0.05, -0.1]
lr = 0.1

nw, nb = gradient_step(weights, biases, dw, db, lr)
for row in nw:
    print([round(v, 4) for v in row])
print([round(v, 4) for v in nb])
`,

	tests: [
		{
			name: "gradient descent updates weights and biases correctly",
			expected: "[0.99, 1.98]\n[2.97, 3.96]\n[0.495, -0.49]\n",
		},
		{
			name: "zero gradient leaves weights unchanged",
			code: `{{FUNC}}
w = [[1.0, 2.0]]
b = [0.5]
nw, nb = gradient_step(w, b, [[0.0, 0.0]], [0.0], 0.5)
print(nw)
print(nb)`,
			expected: "[[1.0, 2.0]]\n[0.5]\n",
		},
		{
			name: "larger learning rate takes bigger step",
			code: `{{FUNC}}
w = [[1.0]]
b = [0.0]
nw1, _ = gradient_step(w, b, [[1.0]], [0.0], 0.1)
nw2, _ = gradient_step(w, b, [[1.0]], [0.0], 0.5)
print(round(nw1[0][0], 4))
print(round(nw2[0][0], 4))`,
			expected: "0.9\n0.5\n",
		},
	],
};
