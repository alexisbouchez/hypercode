import type { Lesson } from "../../types";

export const matrixMultiply: Lesson = {
	id: "matrix-multiply",
	title: "Matrix Multiplication",
	chapterId: "matrices",
	content: `## Matrix Multiplication

Matrix multiplication is **not** element-wise. Each entry of the result is a **dot product** of a row from the left matrix with a column from the right matrix.

For \`A (m×n)\` and \`B (n×p)\`, the result is \`C (m×p)\`:

\`\`\`
C[i,j] = row i of A · column j of B
\`\`\`

\`\`\`python
import numpy as np

A = np.array([[1, 2],
              [3, 4]])

B = np.array([[5, 6],
              [7, 8]])

print(A @ B)
# [[1·5+2·7, 1·6+2·8],   [[19 22]
#  [3·5+4·7, 3·6+4·8]] =  [43 50]]
\`\`\`

### The \`@\` Operator

Python 3.5+ provides \`@\` for matrix multiplication. \`np.dot(A, B)\` also works, but \`@\` is cleaner.

### Shape Rule

The **inner dimensions must match**: if \`A\` is \`(m × n)\` and \`B\` is \`(n × p)\`, then \`A @ B\` is \`(m × p)\`.

\`\`\`python
A.shape  # (2, 3)
B.shape  # (3, 4)
(A @ B).shape  # (2, 4)  ✓

# A.shape = (2, 3), B.shape = (4, 3) → ERROR
\`\`\`

### Non-Commutativity

\`A @ B ≠ B @ A\` in general. Order matters.

### Your Task

Implement \`mat_mul(A, B)\` that returns the matrix product \`A @ B\`.`,

	starterCode: `import numpy as np

def mat_mul(A, B):
    # Return the matrix product A @ B
    pass

A = np.array([[1, 2], [3, 4]])
B = np.array([[5, 6], [7, 8]])
print(mat_mul(A, B))
`,

	solution: `import numpy as np

def mat_mul(A, B):
    return A @ B

A = np.array([[1, 2], [3, 4]])
B = np.array([[5, 6], [7, 8]])
print(mat_mul(A, B))
`,

	tests: [
		{
			name: "[[1,2],[3,4]] @ [[5,6],[7,8]] = [[19,22],[43,50]]",
			expected: "[[19 22]\n [43 50]]\n",
		},
		{
			name: "multiply by identity leaves matrix unchanged",
			code: `{{FUNC}}
A = np.array([[1, 2], [3, 4]])
I = np.eye(2, dtype=int)
print(mat_mul(A, I))`,
			expected: "[[1 2]\n [3 4]]\n",
		},
		{
			name: "output shape is (m, p)",
			code: `{{FUNC}}
A = np.ones((3, 4), dtype=int)
B = np.ones((4, 2), dtype=int)
print(mat_mul(A, B).shape)`,
			expected: "(3, 2)\n",
		},
		{
			name: "row vector times column vector = scalar",
			code: `{{FUNC}}
row = np.array([[1, 2, 3]])       # (1, 3)
col = np.array([[4], [5], [6]])   # (3, 1)
print(mat_mul(row, col))`,
			expected: "[[32]]\n",
		},
	],
};
