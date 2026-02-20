import type { Lesson } from "../../types";

export const matrices: Lesson = {
	id: "matrices",
	title: "Matrices",
	chapterId: "matrices",
	content: `## Matrices in Python

A **matrix** is a 2D array of numbers with \`m\` rows and \`n\` columns — an \`m × n\` matrix. In Python, we represent it as a list of lists.

\`\`\`python
A = [[1, 2, 3],
     [4, 5, 6]]

print(A)           # [[1, 2, 3], [4, 5, 6]]
print(len(A))      # 2  — rows
print(len(A[0]))   # 3  — columns
print(A[0][1])     # 2  — row 0, column 1
print(A[1])        # [4, 5, 6] — entire row 1
\`\`\`

### Special Matrices

\`\`\`python
# Zeros matrix (3×4)
zeros = [[0]*4 for _ in range(3)]

# Identity matrix (3×3)
I = [[1 if i == j else 0 for j in range(3)] for i in range(3)]
# [[1, 0, 0],
#  [0, 1, 0],
#  [0, 0, 1]]
\`\`\`

### The Identity Matrix

The identity matrix \`I\` is the matrix equivalent of the number 1: \`A · I = A\` for any matrix \`A\`. It has 1s on the diagonal and 0s everywhere else.

### Your Task

Implement \`identity(n)\` that returns the \`n × n\` identity matrix as a list of lists.`,

	starterCode: `def identity(n):
    # Return the n×n identity matrix as a list of lists
    pass

print(identity(3))
print(identity(2))
`,

	solution: `def identity(n):
    return [[1 if i == j else 0 for j in range(n)] for i in range(n)]

print(identity(3))
print(identity(2))
`,

	tests: [
		{
			name: "identity(3) and identity(2)",
			expected: "[[1, 0, 0], [0, 1, 0], [0, 0, 1]]\n[[1, 0], [0, 1]]\n",
		},
		{
			name: "identity(1)",
			code: `{{FUNC}}
print(identity(1))`,
			expected: "[[1]]\n",
		},
		{
			name: "size of identity(4) is 4×4",
			code: `{{FUNC}}
I = identity(4)
print(len(I))
print(len(I[0]))`,
			expected: "4\n4\n",
		},
		{
			name: "identity diagonal is all ones",
			code: `{{FUNC}}
I = identity(5)
print(all(I[i][i] == 1 for i in range(5)))`,
			expected: "True\n",
		},
	],
};
