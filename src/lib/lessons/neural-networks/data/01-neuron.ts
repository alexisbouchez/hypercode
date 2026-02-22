import type { Lesson } from "../../types";

export const neuron: Lesson = {
	id: "neuron",
	title: "The Neuron",
	chapterId: "foundations",
	content: `## From Perceptron to Neuron

The perceptron you built in Machine Learning was a binary threshold unit. Real neural networks use a smoother model: the **artificial neuron**.

A neuron computes a **weighted sum** of its inputs plus a **bias**, then passes the result through an activation function:

$$z = \\sum_{i=1}^{n} w_i x_i + b = \\mathbf{w} \\cdot \\mathbf{x} + b$$

$$a = f(z)$$

- $\\mathbf{x}$ — input vector (features)
- $\\mathbf{w}$ — weight vector (learned parameters)
- $b$ — bias (a learnable scalar offset)
- $f$ — activation function (next lesson)
- $z$ — pre-activation (the weighted sum)
- $a$ — activation (the neuron's output)

The **bias** lets the neuron fire even when all inputs are zero, shifting the activation function left or right.

### Why Weighted Sums?

Each weight $w_i$ controls how strongly input $x_i$ influences the output. A large positive weight means "this input strongly activates me". A large negative weight means "this input suppresses me". The bias is a free parameter that controls the threshold.

### Your Task

Implement \`neuron(inputs, weights, bias)\` that returns the pre-activation value $z = \\mathbf{w} \\cdot \\mathbf{x} + b$.

(We will apply the activation function in the next lesson.)`,

	starterCode: `def neuron(inputs, weights, bias):
    # Compute the weighted sum: sum(w_i * x_i) + bias
    return 0.0

print(neuron([1.0, 2.0], [0.5, -1.0], 0.1))   # -1.4
print(neuron([0.0, 0.0], [1.0, 1.0], 1.0))     # 1.0
print(neuron([1.0, 1.0, 1.0], [1.0, 1.0, 1.0], 0.0))  # 3.0
`,

	solution: `def neuron(inputs, weights, bias):
    return sum(w * x for w, x in zip(weights, inputs)) + bias

print(neuron([1.0, 2.0], [0.5, -1.0], 0.1))
print(neuron([0.0, 0.0], [1.0, 1.0], 1.0))
print(neuron([1.0, 1.0, 1.0], [1.0, 1.0, 1.0], 0.0))
`,

	tests: [
		{
			name: "basic weighted sum with bias",
			expected: "-1.4\n1.0\n3.0\n",
		},
		{
			name: "single input neuron",
			code: `{{FUNC}}
print(neuron([3.0], [2.0], -1.0))`,
			expected: "5.0\n",
		},
		{
			name: "zero weights produce bias only",
			code: `{{FUNC}}
print(neuron([5.0, 10.0], [0.0, 0.0], 7.0))`,
			expected: "7.0\n",
		},
		{
			name: "negative weights cancel positive inputs",
			code: `{{FUNC}}
print(neuron([1.0, 1.0], [-1.0, -1.0], 2.0))`,
			expected: "0.0\n",
		},
	],
};
