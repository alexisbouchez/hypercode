import type { Lesson } from "../../types";

export const vectorOps: Lesson = {
	id: "vector-ops",
	title: "Vector Operations",
	chapterId: "vectors",
	content: `## Vector Operations

NumPy applies arithmetic operators **element-wise** across vectors of the same length.

\`\`\`python
import numpy as np

a = np.array([1, 2, 3])
b = np.array([4, 5, 6])

print(a + b)   # [5 7 9]
print(b - a)   # [3 3 3]
print(2 * a)   # [2 4 6]
print(a * b)   # [ 4 10 18]  element-wise product
\`\`\`

### Linear Combination

The most fundamental vector operation is a **linear combination**:

\`\`\`
c = α·a + β·b
\`\`\`

Every vector in the **span** of \`a\` and \`b\` can be expressed this way.

\`\`\`python
alpha, beta = 2, 3
c = alpha * a + beta * b
# 2·[1,2,3] + 3·[4,5,6] = [2,4,6] + [12,15,18] = [14,19,24]
\`\`\`

### Scalar Multiplication

Multiplying a vector by a scalar scales every component. It stretches or shrinks the vector without changing its direction (unless the scalar is negative — then it flips).

### Your Task

Implement \`linear_combination(a, b, alpha, beta)\` that returns \`alpha * a + beta * b\`.`,

	starterCode: `import numpy as np

def linear_combination(a, b, alpha, beta):
    # Return alpha * a + beta * b
    pass

a = np.array([1, 2, 3])
b = np.array([1, 0, -1])
print(linear_combination(a, b, 2, 3))
`,

	solution: `import numpy as np

def linear_combination(a, b, alpha, beta):
    return alpha * a + beta * b

a = np.array([1, 2, 3])
b = np.array([1, 0, -1])
print(linear_combination(a, b, 2, 3))
`,

	tests: [
		{
			name: "2·[1,2,3] + 3·[1,0,-1] = [5,4,3]",
			expected: "[5 4 3]\n",
		},
		{
			name: "1·a + 0·b = a",
			code: `{{FUNC}}
a = np.array([7, 8, 9])
b = np.array([1, 2, 3])
print(linear_combination(a, b, 1, 0))`,
			expected: "[7 8 9]\n",
		},
		{
			name: "0·a + 1·b = b",
			code: `{{FUNC}}
a = np.array([0, 0, 0])
b = np.array([4, 5, 6])
print(linear_combination(a, b, 0, 1))`,
			expected: "[4 5 6]\n",
		},
		{
			name: "negative scale flips sign",
			code: `{{FUNC}}
a = np.array([1, 2, 3])
b = np.array([0, 0, 0])
print(linear_combination(a, b, -1, 0))`,
			expected: "[-1 -2 -3]\n",
		},
	],
};
