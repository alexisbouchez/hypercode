import type { Lesson } from "../../types";

export const matrixMultiply: Lesson = {
	id: "matrix-multiply",
	title: "Matrix Multiplication",
	chapterId: "matrices",
	content: `## Matrix Multiplication

Matrix multiplication is **not** element-wise. Each entry of the result is a **dot product** of a row from the left matrix with a column from the right matrix.

For $\mathbf{A}$ of shape $(m \times n)$ and $\mathbf{B}$ of shape $(n \times p)$, the result $\mathbf{C}$ has shape $(m \times p)$:

$$C_{ij} = \sum_{k=1}^{n} A_{ik} \cdot B_{kj}$$

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

The **inner dimensions must match**: if $\mathbf{A}$ is $(m \times n)$ and $\mathbf{B}$ is $(n \times p)$, then $\mathbf{A}\mathbf{B}$ is $(m \times p)$.

### Non-Commutativity

$\mathbf{A}\mathbf{B} \neq \mathbf{B}\mathbf{A}$ in general. Order matters.

### Your Task

Implement \`mat_mul(A, B)\` that returns the matrix product $\mathbf{A}\mathbf{B}$ as a list of lists.`,

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
