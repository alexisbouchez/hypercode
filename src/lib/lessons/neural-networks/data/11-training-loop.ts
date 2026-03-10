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

### Batch Processing

In practice, we rarely update after *every* sample. Instead, we group samples into **mini-batches**:

- **Batch size 1** (SGD): Noisy gradients, fast updates, good for escaping local minima
- **Full batch**: Stable gradients, but slow and memory-intensive
- **Mini-batch** (typically 32–256): Best of both worlds — stable enough for convergence, small enough for speed

With mini-batches, we average the gradients across the batch before updating weights.

### Convergence

After enough iterations, the weights converge to values that minimize the loss on the training data. You can track this by printing the loss every few epochs.

### Overfitting Prevention

Two key techniques prevent overfitting during training:

**Dropout**: During training, randomly zero out a fraction $p$ of neuron activations each forward pass. This forces the network to learn redundant representations. At test time, all neurons are active but outputs are scaled by $(1-p)$. Typical dropout rates: 0.1–0.5.

**Early stopping**: Monitor validation loss during training. When it stops improving for a set number of epochs (the "patience"), stop training and use the weights from the best epoch. This prevents the network from memorising the training data.

### One Neuron, One Dimension

We will train the simplest possible network: a single sigmoid neuron on 1D data:

$$z = w \\cdot x + b \\qquad a = \\sigma(z) \\qquad \\mathcal{L} = (a - y)^2$$

The backward pass: $\\delta = 2(a - y) \\cdot a(1-a)$, then $\\Delta w = \\delta \\cdot x$, $\\Delta b = \\delta$.

### Your Task

Implement:
- \`train(X, y, lr, epochs)\` — SGD training (one sample at a time)
- \`train_batch(X, y, lr, epochs, batch_size)\` — mini-batch training (average gradients over each batch)

Both initialise $w$ and $b$ to \`0.0\` and return \`(w, b)\` after training.`,

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

def train_batch(X, y, lr=0.5, epochs=20, batch_size=2):
    w = [0.0] * len(X[0])
    b = 0.0
    n = len(X)
    for epoch in range(epochs):
        for start in range(0, n, batch_size):
            batch_X = X[start:start+batch_size]
            batch_y = y[start:start+batch_size]
            bs = len(batch_X)
            # Accumulate gradients over batch, then average and update
            # dw = [0.0] * len(w)
            # db = 0.0
            pass
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

def train_batch(X, y, lr=0.5, epochs=20, batch_size=2):
    w = [0.0] * len(X[0])
    b = 0.0
    n = len(X)
    for epoch in range(epochs):
        for start in range(0, n, batch_size):
            batch_X = X[start:start+batch_size]
            batch_y = y[start:start+batch_size]
            bs = len(batch_X)
            dw = [0.0] * len(w)
            db = 0.0
            for xi, yi in zip(batch_X, batch_y):
                z = sum(wj * xij for wj, xij in zip(w, xi)) + b
                a = sigmoid(z)
                delta = 2 * (a - yi) * a * (1 - a)
                dw = [dwj + delta * xij for dwj, xij in zip(dw, xi)]
                db += delta
            w = [wj - lr * dwj / bs for wj, dwj in zip(w, dw)]
            b = b - lr * db / bs
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
		{
			name: "batch training with batch_size=1 equals SGD",
			code: `{{FUNC}}
X = [[0.0], [1.0]]
y = [0.0, 1.0]
w_sgd, b_sgd = train(X, y, lr=0.5, epochs=20)
w_batch, b_batch = train_batch(X, y, lr=0.5, epochs=20, batch_size=1)
print(round(w_sgd[0], 4) == round(w_batch[0], 4))
print(round(b_sgd, 4) == round(b_batch, 4))`,
			expected: "True\nTrue\n",
		},
		{
			name: "batch training converges on larger dataset",
			code: `{{FUNC}}
X = [[0.0], [0.5], [1.0], [1.5]]
y = [0.0, 0.0, 1.0, 1.0]
w, b = train_batch(X, y, lr=1.0, epochs=50, batch_size=2)
# Should classify correctly
pred_low = sigmoid(w[0]*0.25 + b)
pred_high = sigmoid(w[0]*1.25 + b)
print(pred_low < 0.5)
print(pred_high > 0.5)`,
			expected: "True\nTrue\n",
		},
	],
};
