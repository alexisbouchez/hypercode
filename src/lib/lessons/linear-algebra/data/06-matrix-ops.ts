import type { Lesson } from "../../types";

export const matrixOps: Lesson = {
	id: "matrix-ops",
	title: "Matrix Operations",
	chapterId: "matrices",
	content: `## Matrix Operations

Matrices support the same element-wise operations as vectors, plus **transpose**.

\`\`\`python
import numpy as np

A = np.array([[1, 2], [3, 4]])
B = np.array([[5, 6], [7, 8]])

print(A + B)    # [[6  8]
                #  [10 12]]

print(3 * A)    # [[3  6]
                #  [9  12]]
\`\`\`

### Transpose

The **transpose** \`Aᵀ\` flips a matrix along its diagonal — rows become columns:

\`\`\`python
A = np.array([[1, 2, 3],
              [4, 5, 6]])

print(A.T)
# [[1 4]
#  [2 5]
#  [3 6]]

print(A.shape)    # (2, 3)
print(A.T.shape)  # (3, 2)
\`\`\`

### Symmetric Matrices

A matrix is **symmetric** if \`A = Aᵀ\`. Covariance matrices and Gram matrices are always symmetric. You can check:

\`\`\`python
np.array_equal(A, A.T)
\`\`\`

### Your Task

Implement \`transpose(A)\` that returns the transpose of a matrix.`,

	starterCode: `import numpy as np

def transpose(A):
    # Return the transpose of matrix A
    pass

A = np.array([[1, 2, 3], [4, 5, 6]])
print(transpose(A))
print(transpose(A).shape)
`,

	solution: `import numpy as np

def transpose(A):
    return A.T

A = np.array([[1, 2, 3], [4, 5, 6]])
print(transpose(A))
print(transpose(A).shape)
`,

	tests: [
		{
			name: "transpose [[1,2,3],[4,5,6]] → shape (3,2)",
			expected: "[[1 4]\n [2 5]\n [3 6]]\n(3, 2)\n",
		},
		{
			name: "transpose of square matrix",
			code: `{{FUNC}}
A = np.array([[1, 2], [3, 4]])
print(transpose(A))`,
			expected: "[[1 3]\n [2 4]]\n",
		},
		{
			name: "double transpose = original",
			code: `{{FUNC}}
A = np.array([[1, 2, 3], [4, 5, 6]])
print(np.array_equal(transpose(transpose(A)), A))`,
			expected: "True\n",
		},
		{
			name: "identity is symmetric",
			code: `{{FUNC}}
I = np.eye(4, dtype=int)
print(np.array_equal(transpose(I), I))`,
			expected: "True\n",
		},
	],
};
