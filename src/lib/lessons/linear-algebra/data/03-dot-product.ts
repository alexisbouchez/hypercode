import type { Lesson } from "../../types";

export const dotProduct: Lesson = {
	id: "dot-product",
	title: "Dot Product",
	chapterId: "vectors",
	content: `## The Dot Product

The **dot product** of two vectors multiplies corresponding elements and sums the results:

$$\mathbf{a} \cdot \mathbf{b} = a_0 b_0 + a_1 b_1 + a_2 b_2 + \cdots = \sum_{i=0}^{n-1} a_i b_i$$

\`\`\`python
a = [1, 2, 3]
b = [4, 5, 6]

result = sum(x * y for x, y in zip(a, b))
# 1·4 + 2·5 + 3·6 = 4 + 10 + 18 = 32
print(result)
\`\`\`

### Geometric Meaning

$$\mathbf{a} \cdot \mathbf{b} = \lVert \mathbf{a} \rVert \cdot \lVert \mathbf{b} \rVert \cdot \cos(\theta)$$

Where $\theta$ is the angle between the vectors.

- If $\mathbf{a} \cdot \mathbf{b} = 0$ → vectors are **orthogonal** (perpendicular, 90°)
- If $\mathbf{a} \cdot \mathbf{b} > 0$ → vectors point in similar directions ($\theta < 90°$)
- If $\mathbf{a} \cdot \mathbf{b} < 0$ → vectors point in opposite directions ($\theta > 90°$)

\`\`\`python
# Orthogonal vectors — dot product is 0
x_axis = [1, 0]
y_axis = [0, 1]
print(sum(x*y for x, y in zip(x_axis, y_axis)))  # 0
\`\`\`

### Applications

- **Projection** — how much of one vector lies along another
- **Similarity** — used in cosine similarity for ML and recommendation systems
- **Matrix multiplication** — each output entry is a dot product

### Your Task

Implement \`dot_product(a, b)\` that returns the dot product of two vectors.`,

	starterCode: `def dot_product(a, b):
    # Return the dot product of a and b
    pass

a = [1, 2, 3]
b = [4, 5, 6]
print(dot_product(a, b))
print(dot_product([1, 0], [0, 1]))
`,

	solution: `def dot_product(a, b):
    return sum(x * y for x, y in zip(a, b))

a = [1, 2, 3]
b = [4, 5, 6]
print(dot_product(a, b))
print(dot_product([1, 0], [0, 1]))
`,

	tests: [
		{
			name: "[1,2,3]·[4,5,6] = 32, orthogonal = 0",
			expected: "32\n0\n",
		},
		{
			name: "dot product is commutative",
			code: `{{FUNC}}
a = [3, 4, 5]
b = [1, 2, 3]
print(dot_product(a, b))
print(dot_product(b, a))`,
			expected: "26\n26\n",
		},
		{
			name: "dot with itself = squared norm",
			code: `{{FUNC}}
v = [3, 4]
print(dot_product(v, v))`,
			expected: "25\n",
		},
		{
			name: "anti-parallel vectors give negative dot product",
			code: `{{FUNC}}
a = [1, 0]
b = [-1, 0]
print(dot_product(a, b))`,
			expected: "-1\n",
		},
	],
};
