import type { Lesson } from "../../types";

export const dotProduct: Lesson = {
	id: "dot-product",
	title: "Dot Product",
	chapterId: "vectors",
	content: `## The Dot Product

The **dot product** of two vectors multiplies corresponding elements and sums the results:

\`\`\`
a · b = a₀·b₀ + a₁·b₁ + a₂·b₂ + ...
\`\`\`

\`\`\`python
import numpy as np

a = np.array([1, 2, 3])
b = np.array([4, 5, 6])

print(np.dot(a, b))  # 1·4 + 2·5 + 3·6 = 4 + 10 + 18 = 32
print(a @ b)         # same thing, using the @ operator
\`\`\`

### Geometric Meaning

\`\`\`
a · b = ‖a‖ · ‖b‖ · cos(θ)
\`\`\`

Where \`θ\` is the angle between the vectors.

- If \`a · b = 0\` → vectors are **orthogonal** (perpendicular, 90°)
- If \`a · b > 0\` → vectors point in similar directions (θ < 90°)
- If \`a · b < 0\` → vectors point in opposite directions (θ > 90°)

\`\`\`python
# Orthogonal vectors — dot product is 0
x_axis = np.array([1, 0])
y_axis = np.array([0, 1])
print(np.dot(x_axis, y_axis))  # 0
\`\`\`

### Applications

- **Projection** — how much of one vector lies along another
- **Similarity** — used in cosine similarity for ML and recommendation systems
- **Matrix multiplication** — each output entry is a dot product

### Your Task

Implement \`dot_product(a, b)\` that returns the dot product of two vectors.`,

	starterCode: `import numpy as np

def dot_product(a, b):
    # Return the dot product of a and b
    pass

a = np.array([1, 2, 3])
b = np.array([4, 5, 6])
print(dot_product(a, b))
print(dot_product(np.array([1, 0]), np.array([0, 1])))
`,

	solution: `import numpy as np

def dot_product(a, b):
    return np.dot(a, b)

a = np.array([1, 2, 3])
b = np.array([4, 5, 6])
print(dot_product(a, b))
print(dot_product(np.array([1, 0]), np.array([0, 1])))
`,

	tests: [
		{
			name: "[1,2,3]·[4,5,6] = 32, orthogonal = 0",
			expected: "32\n0\n",
		},
		{
			name: "dot product is commutative",
			code: `{{FUNC}}
a = np.array([3, 4, 5])
b = np.array([1, 2, 3])
print(dot_product(a, b))
print(dot_product(b, a))`,
			expected: "26\n26\n",
		},
		{
			name: "dot with itself = squared norm",
			code: `{{FUNC}}
v = np.array([3, 4])
print(dot_product(v, v))`,
			expected: "25\n",
		},
		{
			name: "anti-parallel vectors give negative dot product",
			code: `{{FUNC}}
a = np.array([1, 0])
b = np.array([-1, 0])
print(dot_product(a, b))`,
			expected: "-1\n",
		},
	],
};
