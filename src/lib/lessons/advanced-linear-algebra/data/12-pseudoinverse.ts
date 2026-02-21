import type { Lesson } from "../../types";

export const pseudoinverseLesson: Lesson = {
	id: "pseudoinverse",
	title: "Pseudoinverse",
	chapterId: "matrix-analysis",
	content: `## Moore-Penrose Pseudoinverse

The **pseudoinverse** $A^+$ generalises the matrix inverse to non-square and singular matrices. It gives the minimum-norm least-squares solution to $Ax = b$:

$$x = A^+ b$$

### For Full Column Rank ($m \\geq n$, rank $n$)

When $A$ has full column rank, the pseudoinverse is:

$$A^+ = (A^T A)^{-1} A^T$$

This is exactly the least-squares formula.

### For Square Invertible $A$

$$A^+ = A^{-1}$$

### Example

$$A = \\begin{pmatrix} 1 & 0 \\\\ 0 & 1 \\\\ 0 & 0 \\end{pmatrix}, \\quad A^T A = \\begin{pmatrix}1&0\\\\0&1\\end{pmatrix}, \\quad A^+ = (A^T A)^{-1} A^T = A^T = \\begin{pmatrix}1&0&0\\\\0&1&0\\end{pmatrix}$$

### Your Task

Implement \`pseudoinverse(A)\` for matrices with full column rank using the formula $A^+ = (A^T A)^{-1} A^T$. The helper \`inv_square\` handles both 1×1 and 2×2 square matrices.`,

	starterCode: `def transpose(A):
    return [[A[j][i] for j in range(len(A))] for i in range(len(A[0]))]

def matmul(A, B):
    m, n, p = len(A), len(A[0]), len(B[0])
    return [[sum(A[i][k] * B[k][j] for k in range(n))
             for j in range(p)] for i in range(m)]

def inv_square(A):
    n = len(A)
    if n == 1:
        return [[1.0 / A[0][0]]]
    det = A[0][0] * A[1][1] - A[0][1] * A[1][0]
    return [[A[1][1]/det, -A[0][1]/det],
            [-A[1][0]/det, A[0][0]/det]]

def pseudoinverse(A):
    At = transpose(A)
    AtA = matmul(At, A)
    AtA_inv = inv_square(AtA)
    return matmul(AtA_inv, At)

A = [[1, 0], [0, 1], [0, 0]]
Ap = pseudoinverse(A)
for row in Ap:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	solution: `def transpose(A):
    return [[A[j][i] for j in range(len(A))] for i in range(len(A[0]))]

def matmul(A, B):
    m, n, p = len(A), len(A[0]), len(B[0])
    return [[sum(A[i][k] * B[k][j] for k in range(n))
             for j in range(p)] for i in range(m)]

def inv_square(A):
    n = len(A)
    if n == 1:
        return [[1.0 / A[0][0]]]
    det = A[0][0] * A[1][1] - A[0][1] * A[1][0]
    return [[A[1][1]/det, -A[0][1]/det],
            [-A[1][0]/det, A[0][0]/det]]

def pseudoinverse(A):
    At = transpose(A)
    AtA = matmul(At, A)
    AtA_inv = inv_square(AtA)
    return matmul(AtA_inv, At)

A = [[1, 0], [0, 1], [0, 0]]
Ap = pseudoinverse(A)
for row in Ap:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	tests: [
		{
			name: "pseudoinverse of [[1,0],[0,1],[0,0]] → [[1,0,0],[0,1,0]]",
			code: `{{FUNC}}
Ap = pseudoinverse([[1, 0], [0, 1], [0, 0]])
for row in Ap:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "1.0000 0.0000 0.0000\n0.0000 1.0000 0.0000\n",
		},
		{
			name: "A⁺ A = I for [[1,0],[0,1],[0,0]]",
			code: `{{FUNC}}
A = [[1, 0], [0, 1], [0, 0]]
Ap = pseudoinverse(A)
ApA = matmul(Ap, A)
for row in ApA:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "1.0000 0.0000\n0.0000 1.0000\n",
		},
		{
			name: "pseudoinverse of [[2],[3]] = [[2/13, 3/13]]",
			code: `{{FUNC}}
Ap = pseudoinverse([[2], [3]])
for row in Ap:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "0.1538 0.2308\n",
		},
		{
			name: "pseudoinverse of square [[2,0],[0,4]] = [[0.5,0],[0,0.25]]",
			code: `{{FUNC}}
Ap = pseudoinverse([[2, 0], [0, 4]])
for row in Ap:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "0.5000 0.0000\n0.0000 0.2500\n",
		},
	],
};
