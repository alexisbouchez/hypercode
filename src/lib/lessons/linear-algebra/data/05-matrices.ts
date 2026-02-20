import type { Lesson } from "../../types";

export const matrices: Lesson = {
	id: "matrices",
	title: "Matrices",
	chapterId: "matrices",
	content: `## Matrices in NumPy

A **matrix** is a 2D array of numbers with \`m\` rows and \`n\` columns — an \`m × n\` matrix.

\`\`\`python
import numpy as np

A = np.array([[1, 2, 3],
              [4, 5, 6]])

print(A)        # [[1 2 3]
                #  [4 5 6]]
print(A.shape)  # (2, 3)  — 2 rows, 3 columns
print(A[0, 1])  # 2       — row 0, column 1
print(A[1, :])  # [4 5 6] — entire row 1
\`\`\`

### Special Matrices

\`\`\`python
# Zeros matrix
np.zeros((3, 4))        # 3×4 matrix of 0.0

# Ones matrix
np.ones((2, 2))         # 2×2 matrix of 1.0

# Identity matrix
np.eye(3, dtype=int)    # 3×3 identity
# [[1 0 0]
#  [0 1 0]
#  [0 0 1]]
\`\`\`

### The Identity Matrix

The identity matrix \`I\` is the matrix equivalent of the number 1: \`A · I = A\` for any matrix \`A\`. It has 1s on the diagonal and 0s everywhere else.

### Your Task

Implement \`identity(n)\` that returns the \`n × n\` identity matrix as an integer array.`,

	starterCode: `import numpy as np

def identity(n):
    # Return the n×n identity matrix with integer dtype
    pass

print(identity(3))
print(identity(2))
`,

	solution: `import numpy as np

def identity(n):
    return np.eye(n, dtype=int)

print(identity(3))
print(identity(2))
`,

	tests: [
		{
			name: "identity(3) and identity(2)",
			expected: "[[1 0 0]\n [0 1 0]\n [0 0 1]]\n[[1 0]\n [0 1]]\n",
		},
		{
			name: "identity(1)",
			code: `{{FUNC}}
print(identity(1))`,
			expected: "[[1]]\n",
		},
		{
			name: "shape of identity(4)",
			code: `{{FUNC}}
print(identity(4).shape)`,
			expected: "(4, 4)\n",
		},
		{
			name: "identity diagonal is all ones",
			code: `{{FUNC}}
I = identity(5)
print(all(I[i, i] == 1 for i in range(5)))`,
			expected: "True\n",
		},
	],
};
