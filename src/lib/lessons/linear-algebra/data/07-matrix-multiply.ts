import type { Lesson } from "../../types";

export const matrixMultiply: Lesson = {
	id: "matrix-multiply",
	title: "Matrix Multiplication",
	chapterId: "matrices",
	content: `## Matrix Multiplication

Matrix multiplication is **not** element-wise. Each entry of the result is a **dot product** of a row from the left matrix with a column from the right matrix.

For \`A (m×n)\` and \`B (n×p)\`, the result is \`C (m×p)\`:

\`\`\`
C[i][j] = row i of A · column j of B
\`\`\`

\`\`\`python
A = [[1, 2],
     [3, 4]]

B = [[5, 6],
     [7, 8]]

def mat_mul(A, B):
    m, n, p = len(A), len(A[0]), len(B[0])
    return [[sum(A[i][k] * B[k][j] for k in range(n))
             for j in range(p)]
            for i in range(m)]

print(mat_mul(A, B))
# [[1·5+2·7, 1·6+2·8],   [[19, 22],
#  [3·5+4·7, 3·6+4·8]] =  [43, 50]]
\`\`\`

### Shape Rule

The **inner dimensions must match**: if \`A\` is \`(m × n)\` and \`B\` is \`(n × p)\`, then \`A @ B\` is \`(m × p)\`.

### Non-Commutativity

\`A @ B ≠ B @ A\` in general. Order matters.

### Your Task

Implement \`mat_mul(A, B)\` that returns the matrix product \`A @ B\` as a list of lists.`,

	starterCode: `def mat_mul(A, B):
    # Return the matrix product A @ B as a list of lists
    pass

A = [[1, 2], [3, 4]]
B = [[5, 6], [7, 8]]
print(mat_mul(A, B))
`,

	solution: `def mat_mul(A, B):
    m, n, p = len(A), len(A[0]), len(B[0])
    return [[sum(A[i][k] * B[k][j] for k in range(n))
             for j in range(p)]
            for i in range(m)]

A = [[1, 2], [3, 4]]
B = [[5, 6], [7, 8]]
print(mat_mul(A, B))
`,

	tests: [
		{
			name: "[[1,2],[3,4]] @ [[5,6],[7,8]] = [[19,22],[43,50]]",
			expected: "[[19, 22], [43, 50]]\n",
		},
		{
			name: "multiply by identity leaves matrix unchanged",
			code: `{{FUNC}}
A = [[1, 2], [3, 4]]
I = [[1, 0], [0, 1]]
print(mat_mul(A, I))`,
			expected: "[[1, 2], [3, 4]]\n",
		},
		{
			name: "output shape is (m, p)",
			code: `{{FUNC}}
A = [[1]*4 for _ in range(3)]   # 3×4
B = [[1]*2 for _ in range(4)]   # 4×2
C = mat_mul(A, B)
print(len(C))
print(len(C[0]))`,
			expected: "3\n2\n",
		},
		{
			name: "row vector times column vector = scalar",
			code: `{{FUNC}}
row = [[1, 2, 3]]         # 1×3
col = [[4], [5], [6]]     # 3×1
print(mat_mul(row, col))`,
			expected: "[[32]]\n",
		},
	],
};
