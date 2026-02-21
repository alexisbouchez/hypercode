import type { Lesson } from "../../types";

export const luDecomp: Lesson = {
	id: "lu-decomp",
	title: "LU Decomposition",
	chapterId: "decompositions",
	content: `## LU Decomposition

**LU decomposition** factors a matrix $A$ into a lower-triangular matrix $L$ and an upper-triangular matrix $U$:

$$A = LU$$

This is the matrix form of **Gaussian elimination** — $L$ records the row operations and $U$ is the result.

### Doolittle's Algorithm

For an $n \\times n$ matrix:

- $U$ has the same rows as the row-echelon form of $A$
- $L$ is unit lower-triangular (diagonal of 1s), with:

$$L_{ik} = \\frac{A_{ik} - \\sum_{j=0}^{k-1} L_{ij} U_{jk}}{U_{kk}}$$

$$U_{kj} = A_{kj} - \\sum_{i=0}^{k-1} L_{ki} U_{ij}$$

### Example

$$A = \\begin{pmatrix} 2 & 1 \\\\ 6 & 4 \\end{pmatrix} = \\begin{pmatrix}1 & 0 \\\\ 3 & 1\\end{pmatrix}\\begin{pmatrix}2 & 1 \\\\ 0 & 1\\end{pmatrix} = LU$$

**Verify**: $3 \\cdot 2 + 1 \\cdot 0 = 6$ ✓, $3 \\cdot 1 + 1 \\cdot 1 = 4$ ✓

### Applications

Once you have $A = LU$, solving $Ax = b$ becomes two easy triangular solves:

1. Solve $Ly = b$ (forward substitution)
2. Solve $Ux = y$ (back substitution)

This is much faster than recomputing Gaussian elimination for every new right-hand side $b$.

\`\`\`python
def lu_decomp(A):
    n = len(A)
    L = [[1.0 if i==j else 0.0 for j in range(n)] for i in range(n)]
    U = [row[:] for row in A]
    for k in range(n):
        for i in range(k+1, n):
            L[i][k] = U[i][k] / U[k][k]
            for j in range(k, n):
                U[i][j] -= L[i][k] * U[k][j]
    return L, U
\`\`\`

### Your Task

Implement \`lu_decomp(A)\` using Doolittle's algorithm. Return \`(L, U)\`.`,

	starterCode: `def lu_decomp(A):
    n = len(A)
    L = [[1.0 if i==j else 0.0 for j in range(n)] for i in range(n)]
    U = [row[:] for row in A]
    # eliminate below each pivot using L to record multipliers
    return L, U

A = [[2, 1], [6, 4]]
L, U = lu_decomp(A)
print(round(L[1][0], 4))   # multiplier: 6/2 = 3.0
print(round(U[1][1], 4))   # pivot after elimination: 1.0
`,

	solution: `def lu_decomp(A):
    n = len(A)
    L = [[1.0 if i==j else 0.0 for j in range(n)] for i in range(n)]
    U = [[float(x) for x in row] for row in A]
    for k in range(n):
        for i in range(k + 1, n):
            L[i][k] = U[i][k] / U[k][k]
            for j in range(k, n):
                U[i][j] -= L[i][k] * U[k][j]
    return L, U

A = [[2, 1], [6, 4]]
L, U = lu_decomp(A)
print(round(L[1][0], 4))
print(round(U[1][1], 4))
`,

	tests: [
		{
			name: "L[1][0]=3.0 (multiplier), U[1][1]=1.0 (pivot)",
			expected: "3.0\n1.0\n",
		},
		{
			name: "L@U reconstructs A",
			code: `{{FUNC}}
A = [[2, 1], [6, 4]]
L, U = lu_decomp(A)
n = len(A)
LU = [[sum(L[i][k]*U[k][j] for k in range(n)) for j in range(n)] for i in range(n)]
print(round(LU[1][0], 4))
print(round(LU[1][1], 4))`,
			expected: "6.0\n4.0\n",
		},
		{
			name: "3×3 LU: L[2][0] and L[2][1]",
			code: `{{FUNC}}
A = [[2,1,1],[4,3,3],[8,7,9]]
L, U = lu_decomp(A)
print(round(L[2][0], 4))
print(round(L[2][1], 4))`,
			expected: "4.0\n3.0\n",
		},
		{
			name: "identity: L=I, U=I",
			code: `{{FUNC}}
I = [[1,0],[0,1]]
L, U = lu_decomp(I)
print(round(L[1][0], 4))
print(round(U[0][0], 4))`,
			expected: "0.0\n1.0\n",
		},
	],
};
