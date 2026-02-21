import type { Lesson } from "../../types";

export const perceptron: Lesson = {
	id: "perceptron",
	title: "The Perceptron",
	chapterId: "neural-networks",
	content: `## The Perceptron

The **perceptron** is the simplest neural network unit — a single binary classifier. It computes a weighted sum of its inputs and fires (outputs 1) if the sum exceeds a threshold:

$$\\hat{y} = \\begin{cases} 1 & \\text{if } \\mathbf{w} \\cdot \\mathbf{x} + b > 0 \\\\ 0 & \\text{otherwise} \\end{cases}$$

### Learning Rule

The perceptron learning rule updates the weights whenever a prediction is wrong:

$$\\text{error} = y_{\\text{true}} - \\hat{y}$$

$$w_i \\leftarrow w_i + \\alpha \\cdot \\text{error} \\cdot x_i \\qquad \\forall i$$

$$b \\leftarrow b + \\alpha \\cdot \\text{error}$$

where $\\alpha$ is the learning rate.

- If the prediction is correct: error = 0, no update
- If the true label is 1 but we predicted 0: error = +1, weights increase
- If the true label is 0 but we predicted 1: error = −1, weights decrease

### Historical Note

Rosenblatt's 1958 perceptron was the first trainable neural model. Minsky & Papert (1969) showed it cannot learn XOR — motivating multi-layer networks.

### Your Task

Implement:
- \`perceptron_output(x, w, b)\` → 1 if dot(x,w)+b > 0 else 0
- \`perceptron_update(w, b, x, y_true, lr)\` → (new_w, new_b)`,

	starterCode: `def dot(x, w):
    return sum(xi * wi for xi, wi in zip(x, w))

def perceptron_output(x, w, b):
    # 1 if dot(x,w) + b > 0 else 0
    return 0

def perceptron_update(w, b, x, y_true, lr):
    # error = y_true - perceptron_output
    # w[i] += lr * error * x[i]; b += lr * error
    return (w, b)

print(perceptron_output([1,1], [0.5,0.5], -0.5))   # 1
print(perceptron_output([1,1], [0.5,0.5], -1.5))   # 0
nw, nb = perceptron_update([0.0, 0.0], 0.0, [1.0, 1.0], 1, 0.1)
print(nw)          # [0.1, 0.1]
print(round(nb, 4))  # 0.1
`,

	solution: `def dot(x, w):
    return sum(xi * wi for xi, wi in zip(x, w))

def perceptron_output(x, w, b):
    return 1 if dot(x, w) + b > 0 else 0

def perceptron_update(w, b, x, y_true, lr):
    error = y_true - perceptron_output(x, w, b)
    new_w = [w[i] + lr * error * x[i] for i in range(len(w))]
    new_b = b + lr * error
    return (new_w, new_b)

print(perceptron_output([1,1], [0.5,0.5], -0.5))
print(perceptron_output([1,1], [0.5,0.5], -1.5))
nw, nb = perceptron_update([0.0, 0.0], 0.0, [1.0, 1.0], 1, 0.1)
print(nw)
print(round(nb, 4))
`,

	tests: [
		{
			name: "perceptron_output fires/not-fires, update rule correct",
			expected: "1\n0\n[0.1, 0.1]\n0.1\n",
		},
		{
			name: "perceptron_output with zero bias fires when dot > 0",
			code: `{{FUNC}}
print(perceptron_output([0,0], [1,1], 0.5))`,
			expected: "1\n",
		},
		{
			name: "perceptron_update no-op when prediction is correct",
			code: `{{FUNC}}
w = [1.0, 0.0]
nw, nb = perceptron_update(w, 0.0, [1.0, 0.0], 1, 0.1)
print(nw)
print(round(nb, 4))`,
			expected: "[1.0, 0.0]\n0.0\n",
		},
		{
			name: "perceptron_update decreases weights on false positive",
			code: `{{FUNC}}
nw, nb = perceptron_update([1.0, 1.0], 0.0, [1.0, 1.0], 0, 0.1)
print([round(v, 4) for v in nw])
print(round(nb, 4))`,
			expected: "[0.9, 0.9]\n-0.1\n",
		},
	],
};
