import type { Lesson } from "../../types";

export const choleskyLesson: Lesson = {
	id: "cholesky",
	title: "Cholesky Decomposition",
	chapterId: "decompositions",
	content: `## Cholesky Decomposition

For a **symmetric positive definite (SPD)** matrix $A$, the Cholesky decomposition gives:

$$A = L L^T$$

where $L$ is lower triangular with positive diagonal entries.

### Why Cholesky?

- Twice as fast as LU (exploits symmetry)
- Numerically more stable
- The positive diagonal entries confirm $A$ is truly SPD

### Algorithm

$$L_{ij} = \\frac{A_{ij} - \\sum_{k < j} L_{ik}\\, L_{jk}}{L_{jj}} \\quad (i > j, \\text{ off-diagonal})$$

$$L_{jj} = \\sqrt{A_{jj} - \\sum_{k < j} L_{jk}^2} \\quad (\\text{diagonal})$$

### Example

$$A = \\begin{pmatrix} 4 & 2 & 2 \\\\ 2 & 5 & 3 \\\\ 2 & 3 & 6 \\end{pmatrix}, \\quad L = \\begin{pmatrix} 2 & 0 & 0 \\\\ 1 & 2 & 0 \\\\ 1 & 1 & 2 \\end{pmatrix}$$

Check: $L L^T = A$.

### Your Task

Implement \`cholesky(A)\` returning the lower triangular factor L.`,

	starterCode: `import math

def cholesky(A):
    n = len(A)
    L = [[0.0] * n for _ in range(n)]
    for i in range(n):
        for j in range(i + 1):
            s = sum(L[i][k] * L[j][k] for k in range(j))
            if i == j:
                L[i][j] = math.sqrt(A[i][i] - s)
            else:
                L[i][j] = 0.0  # replace with correct formula
    return L

A = [[4, 2, 2], [2, 5, 3], [2, 3, 6]]
L = cholesky(A)
for row in L:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	solution: `import math

def cholesky(A):
    n = len(A)
    L = [[0.0] * n for _ in range(n)]
    for i in range(n):
        for j in range(i + 1):
            s = sum(L[i][k] * L[j][k] for k in range(j))
            if i == j:
                L[i][j] = math.sqrt(A[i][i] - s)
            else:
                L[i][j] = (A[i][j] - s) / L[j][j]
    return L

A = [[4, 2, 2], [2, 5, 3], [2, 3, 6]]
L = cholesky(A)
for row in L:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	tests: [
		{
			name: "cholesky([[4,2,2],[2,5,3],[2,3,6]]) — first row",
			code: `{{FUNC}}
L = cholesky([[4, 2, 2], [2, 5, 3], [2, 3, 6]])
print(" ".join(f"{x:.4f}" for x in L[0]))`,
			expected: "2.0000 0.0000 0.0000\n",
		},
		{
			name: "cholesky([[4,2,2],[2,5,3],[2,3,6]]) — second row",
			code: `{{FUNC}}
L = cholesky([[4, 2, 2], [2, 5, 3], [2, 3, 6]])
print(" ".join(f"{x:.4f}" for x in L[1]))`,
			expected: "1.0000 2.0000 0.0000\n",
		},
		{
			name: "cholesky([[4,2,2],[2,5,3],[2,3,6]]) — third row",
			code: `{{FUNC}}
L = cholesky([[4, 2, 2], [2, 5, 3], [2, 3, 6]])
print(" ".join(f"{x:.4f}" for x in L[2]))`,
			expected: "1.0000 1.0000 2.0000\n",
		},
		{
			name: "cholesky([[9,3],[3,5]]) diagonal",
			code: `{{FUNC}}
L = cholesky([[9, 3], [3, 5]])
print(f"{L[0][0]:.4f}")
print(f"{L[1][1]:.4f}")`,
			expected: "3.0000\n2.0000\n",
		},
	],
};
