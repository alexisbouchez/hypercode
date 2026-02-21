import type { Lesson } from "../../types";

export const linear: Lesson = {
	id: "linear",
	title: "Linear Layer",
	chapterId: "primitives",
	content: `## The Linear Layer

A linear layer computes \`y = Wx\` — a matrix-vector product. It is the fundamental building block of every neural network.

### Matrix-Vector Multiplication

Given weight matrix \`W\` (rows × cols) and input vector \`x\` (cols):

\`\`\`
y[i] = sum(W[i][j] * x[j] for j in range(cols))
\`\`\`

Each output element is the **dot product** of one row of W with x.

### The Two-Line Implementation

In MicroGPT, this is written as a list comprehension:

\`\`\`python
def linear(x, w):
    return [sum(wi * xi for wi, xi in zip(wo, x)) for wo in w]
\`\`\`

This works with both plain floats and \`Value\` objects — the arithmetic operations are the same either way.

### Example

\`\`\`python
x = [1.0, 0.0]
w = [[1.0, 2.0],
     [3.0, 4.0]]

result = linear(x, w)
# row 0: 1*1 + 2*0 = 1.0
# row 1: 3*1 + 4*0 = 3.0
# result = [1.0, 3.0]
\`\`\`

### The GPT uses 7 linear layers

- \`attn_wq\`, \`attn_wk\`, \`attn_wv\`: project tokens to queries, keys, values
- \`attn_wo\`: project attention output back
- \`mlp_fc1\`, \`mlp_fc2\`: the two-layer feedforward block
- \`lm_head\`: project hidden state to vocabulary logits

### Your Task

Implement \`linear(x, w)\`.`,

	starterCode: `def linear(x, w):
    # TODO: return [dot(row, x) for row in w]
    # Each element of the output is: sum(w[i][j] * x[j] for j in range(len(x)))
    pass

x = [1.0, 0.0]
w = [[1.0, 2.0], [3.0, 4.0]]
print(linear(x, w))

x2 = [1.0, 1.0]
print(linear(x2, w))
`,

	solution: `def linear(x, w):
    return [sum(wi * xi for wi, xi in zip(wo, x)) for wo in w]

x = [1.0, 0.0]
w = [[1.0, 2.0], [3.0, 4.0]]
print(linear(x, w))

x2 = [1.0, 1.0]
print(linear(x2, w))
`,

	tests: [
		{
			name: "linear computes matrix-vector product correctly",
			expected: "[1.0, 3.0]\n[3.0, 7.0]\n",
		},
	],
};
