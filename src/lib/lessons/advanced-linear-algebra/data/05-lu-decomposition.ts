import type { Lesson } from "../../types";

export const luDecompositionLesson: Lesson = {
	id: "lu-decomposition",
	title: "LU Decomposition",
	chapterId: "decompositions",
	content: `## LU Decomposition

Every square matrix A (with non-zero pivots) can be factored as **A = LU** where:
- **L** is lower triangular with 1s on the diagonal
- **U** is upper triangular

This is Gaussian elimination written as a matrix product.

### Algorithm

For each pivot column k, eliminate below the diagonal by subtracting scaled rows:

\`\`\`
factor = U[i][k] / U[k][k]
L[i][k] = factor
U[i] -= factor * U[k]
\`\`\`

The multipliers become the entries of L.

### Example

\`\`\`
A = [[2, 1, 1],   L = [[1, 0, 0],   U = [[2, 1, 1],
     [4, 3, 3],        [2, 1, 0],        [0, 1, 1],
     [8, 7, 9]]        [4, 3, 1]]        [0, 0, 2]]
\`\`\`

Verify: L × U = A.

### Why LU?

Once A = LU, solving Ax = b costs O(n²) — just two triangular solves — instead of rerunning Gaussian elimination for each new b.

### Your Task

Implement \`lu_decompose(A)\` returning \`(L, U)\`. Modify a copy of A in-place for U while recording multipliers in L.`,

	starterCode: `def lu_decompose(A):
    n = len(A)
    L = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
    U = [row[:] for row in A]
    for k in range(n):
        for i in range(k + 1, n):
            # Compute factor and store in L
            factor = 0.0
            L[i][k] = factor
            for j in range(k, n):
                U[i][j] -= factor * U[k][j]
    return L, U

A = [[2, 1, 1], [4, 3, 3], [8, 7, 9]]
L, U = lu_decompose(A)
for row in L:
    print(" ".join(f"{x:.4f}" for x in row))
print("---")
for row in U:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	solution: `def lu_decompose(A):
    n = len(A)
    L = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
    U = [row[:] for row in A]
    for k in range(n):
        for i in range(k + 1, n):
            factor = U[i][k] / U[k][k]
            L[i][k] = factor
            for j in range(k, n):
                U[i][j] -= factor * U[k][j]
    return L, U

A = [[2, 1, 1], [4, 3, 3], [8, 7, 9]]
L, U = lu_decompose(A)
for row in L:
    print(" ".join(f"{x:.4f}" for x in row))
print("---")
for row in U:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	tests: [
		{
			name: "L matrix of [[2,1,1],[4,3,3],[8,7,9]]",
			code: `{{FUNC}}
L, U = lu_decompose([[2, 1, 1], [4, 3, 3], [8, 7, 9]])
for row in L:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "1.0000 0.0000 0.0000\n2.0000 1.0000 0.0000\n4.0000 3.0000 1.0000\n",
		},
		{
			name: "U matrix of [[2,1,1],[4,3,3],[8,7,9]]",
			code: `{{FUNC}}
L, U = lu_decompose([[2, 1, 1], [4, 3, 3], [8, 7, 9]])
for row in U:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "2.0000 1.0000 1.0000\n0.0000 1.0000 1.0000\n0.0000 0.0000 2.0000\n",
		},
		{
			name: "LU = A (reconstruction check)",
			code: `{{FUNC}}
A = [[2, 1, 1], [4, 3, 3], [8, 7, 9]]
L, U = lu_decompose(A)
n = len(A)
LU = [[sum(L[i][k]*U[k][j] for k in range(n)) for j in range(n)] for i in range(n)]
for row in LU:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "2.0000 1.0000 1.0000\n4.0000 3.0000 3.0000\n8.0000 7.0000 9.0000\n",
		},
		{
			name: "LU of 2×2 [[4,3],[6,3]]",
			code: `{{FUNC}}
L, U = lu_decompose([[4, 3], [6, 3]])
for row in L:
    print(" ".join(f"{x:.4f}" for x in row))
for row in U:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "1.0000 0.0000\n1.5000 1.0000\n4.0000 3.0000\n0.0000 -1.5000\n",
		},
	],
};
