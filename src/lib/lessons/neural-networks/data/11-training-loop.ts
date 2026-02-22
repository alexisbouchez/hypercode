import type { Lesson } from "../../types";

export const trainingLoop: Lesson = {
	id: "training-loop",
	title: "The Training Loop",
	chapterId: "training",
	content: `## Putting It Together

The training loop is the core of all neural network learning:

\`\`\`
for epoch in range(num_epochs):
    for (x, y) in training_data:
        1. Forward pass  → compute prediction a
        2. Compute loss  → L = (a - y)²
        3. Backward pass → compute gradients dw, db
        4. Update params → w -= lr * dw; b -= lr * db
\`\`\`

This is **stochastic gradient descent** (SGD) — we update after every single example.

### Convergence

After enough iterations, the weights converge to values that minimize the loss on the training data. You can track this by printing the loss every few epochs.

### One Neuron, One Dimension

We will train the simplest possible network: a single sigmoid neuron on 1D data:

$$z = w \\cdot x + b \\qquad a = \\sigma(z) \\qquad \\mathcal{L} = (a - y)^2$$

The backward pass: $\\delta = 2(a - y) \\cdot a(1-a)$, then $\\Delta w = \\delta \\cdot x$, $\\Delta b = \\delta$.

### Your Task

Implement \`train(X, y, lr, epochs)\` that:
1. Initialises $w$ and $b$ to \`0.0\`
2. Loops over epochs, and within each epoch loops over samples
3. For each sample: forward, backward, update
4. Returns \`(w, b)\` after training`,

	starterCode: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def train(X, y, lr=0.5, epochs=20):
    w = [0.0] * len(X[0])
    b = 0.0
    for epoch in range(epochs):
        for xi, yi in zip(X, y):
            # Forward
            z = sum(wj * xij for wj, xij in zip(w, xi)) + b
            a = sigmoid(z)
            # Backward: delta = 2*(a-yi)*a*(1-a)
            delta = 0.0
            # Update
            w = w
            b = b
    return w, b

X = [[0.0], [1.0]]
y = [0.0, 1.0]
w, b = train(X, y, lr=0.5, epochs=20)
print(round(w[0], 4))   # 1.6432
print(round(b, 4))       # -0.5592
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def train(X, y, lr=0.5, epochs=20):
    w = [0.0] * len(X[0])
    b = 0.0
    for epoch in range(epochs):
        for xi, yi in zip(X, y):
            z = sum(wj * xij for wj, xij in zip(w, xi)) + b
            a = sigmoid(z)
            delta = 2 * (a - yi) * a * (1 - a)
            w = [wj - lr * delta * xij for wj, xij in zip(w, xi)]
            b = b - lr * delta
    return w, b

X = [[0.0], [1.0]]
y = [0.0, 1.0]
w, b = train(X, y, lr=0.5, epochs=20)
print(round(w[0], 4))
print(round(b, 4))
`,

	tests: [
		{
			name: "training reduces error over 20 epochs",
			expected: "1.6432\n-0.5592\n",
		},
		{
			name: "trained network predicts reasonable outputs",
			code: `{{FUNC}}
X = [[0.0], [1.0]]
y = [0.0, 1.0]
w, b = train(X, y, lr=0.5, epochs=20)
pred0 = sigmoid(w[0]*0.0 + b)
pred1 = sigmoid(w[0]*1.0 + b)
print(round(pred0, 4))
print(round(pred1, 4))`,
			expected: "0.3637\n0.7473\n",
		},
		{
			name: "longer training converges further",
			code: `{{FUNC}}
X = [[0.0], [1.0]]
y = [0.0, 1.0]
w20, b20 = train(X, y, lr=0.5, epochs=20)
w50, b50 = train(X, y, lr=0.5, epochs=50)
# Loss should be lower after more epochs
loss20 = (sigmoid(w20[0]*1+b20)-1)**2 + (sigmoid(w20[0]*0+b20)-0)**2
loss50 = (sigmoid(w50[0]*1+b50)-1)**2 + (sigmoid(w50[0]*0+b50)-0)**2
print(loss50 < loss20)`,
			expected: "True\n",
		},
	],
};
