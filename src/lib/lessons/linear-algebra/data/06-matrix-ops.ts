import type { Lesson } from "../../types";

export const matrixOps: Lesson = {
	id: "matrix-ops",
	title: "Matrix Operations",
	chapterId: "matrices",
	content: `## Matrix Operations

Matrices support element-wise operations and **transpose**.

\`\`\`python
A = [[1, 2], [3, 4]]
B = [[5, 6], [7, 8]]

# Element-wise addition
add = [[A[i][j] + B[i][j] for j in range(len(A[0]))] for i in range(len(A))]
print(add)    # [[6, 8], [10, 12]]

# Scalar multiplication
scaled = [[3 * A[i][j] for j in range(len(A[0]))] for i in range(len(A))]
print(scaled) # [[3, 6], [9, 12]]
\`\`\`

### Transpose

The **transpose** \`Aᵀ\` flips a matrix along its diagonal — rows become columns:

\`\`\`python
A = [[1, 2, 3],
     [4, 5, 6]]

AT = [[A[i][j] for i in range(len(A))] for j in range(len(A[0]))]
print(AT)
# [[1, 4], [2, 5], [3, 6]]
# rows: len(AT) = 3, cols: len(AT[0]) = 2
\`\`\`

### Symmetric Matrices

A matrix is **symmetric** if \`A = Aᵀ\`. Covariance matrices and Gram matrices are always symmetric.

### Your Task

Implement \`transpose(A)\` that returns the transpose of a matrix as a list of lists.`,

	starterCode: `def transpose(A):
    # Return the transpose of matrix A as a list of lists
    pass

A = [[1, 2, 3], [4, 5, 6]]
print(transpose(A))
T = transpose(A)
print(len(T))
print(len(T[0]))
`,

	solution: `def transpose(A):
    rows, cols = len(A), len(A[0])
    return [[A[i][j] for i in range(rows)] for j in range(cols)]

A = [[1, 2, 3], [4, 5, 6]]
print(transpose(A))
T = transpose(A)
print(len(T))
print(len(T[0]))
`,

	tests: [
		{
			name: "transpose [[1,2,3],[4,5,6]] → shape 3×2",
			expected: "[[1, 4], [2, 5], [3, 6]]\n3\n2\n",
		},
		{
			name: "transpose of square matrix",
			code: `{{FUNC}}
A = [[1, 2], [3, 4]]
print(transpose(A))`,
			expected: "[[1, 3], [2, 4]]\n",
		},
		{
			name: "double transpose = original",
			code: `{{FUNC}}
A = [[1, 2, 3], [4, 5, 6]]
print(transpose(transpose(A)) == A)`,
			expected: "True\n",
		},
		{
			name: "identity is symmetric",
			code: `{{FUNC}}
I = [[1 if i==j else 0 for j in range(4)] for i in range(4)]
print(transpose(I) == I)`,
			expected: "True\n",
		},
	],
};
