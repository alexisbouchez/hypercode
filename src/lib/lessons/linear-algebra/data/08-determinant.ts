import type { Lesson } from "../../types";

export const determinant: Lesson = {
	id: "determinant",
	title: "Determinant",
	chapterId: "matrices",
	content: `## The Determinant

The **determinant** of a square matrix is a single number that encodes how the matrix scales area (2D) or volume (3D).

For a 2×2 matrix:
\`\`\`
det([[a, b], [c, d]]) = a·d - b·c
\`\`\`

\`\`\`python
def det(A):
    if len(A) == 1:
        return A[0][0]
    if len(A) == 2:
        return A[0][0]*A[1][1] - A[0][1]*A[1][0]
    # Cofactor expansion along first row
    return sum(
        ((-1)**j) * A[0][j] * det([[A[i][k] for k in range(len(A)) if k != j]
                                    for i in range(1, len(A))])
        for j in range(len(A))
    )

A = [[3, 0], [0, 2]]
print(int(round(det(A))))  # 6  — scales area by 6
\`\`\`

### What the Determinant Tells You

| det(A) | Meaning |
|--------|---------|
| = 0    | Matrix is **singular** — no inverse exists, squishes space to a lower dimension |
| > 0    | Preserves orientation |
| < 0    | Flips orientation |
| = 1    | Preserves area/volume |

### Singular Matrices

If rows are linearly dependent (one row is a multiple of another), the determinant is 0:

\`\`\`python
S = [[1, 2], [2, 4]]  # row 2 = 2 × row 1
print(int(round(det(S))))  # 0
\`\`\`

### Your Task

Implement \`det(A)\` that returns the determinant as an integer (rounded).`,

	starterCode: `def det(A):
    # Return the determinant as a rounded integer
    # Hint: for 2x2 use a*d - b*c; for larger use cofactor expansion
    pass

print(det([[3, 0], [0, 2]]))
print(det([[1, 2], [3, 4]]))
print(det([[1, 2], [2, 4]]))
`,

	solution: `def det(A):
    n = len(A)
    if n == 1:
        return A[0][0]
    if n == 2:
        return int(round(A[0][0]*A[1][1] - A[0][1]*A[1][0]))
    return int(round(sum(
        ((-1)**j) * A[0][j] * det([[A[i][k] for k in range(n) if k != j]
                                    for i in range(1, n)])
        for j in range(n)
    )))

print(det([[3, 0], [0, 2]]))
print(det([[1, 2], [3, 4]]))
print(det([[1, 2], [2, 4]]))
`,

	tests: [
		{
			name: "diagonal, 2x2, singular",
			expected: "6\n-2\n0\n",
		},
		{
			name: "identity has det 1",
			code: `{{FUNC}}
I = [[1 if i==j else 0 for j in range(3)] for i in range(3)]
print(det(I))`,
			expected: "1\n",
		},
		{
			name: "swap rows negates determinant",
			code: `{{FUNC}}
A = [[1, 2], [3, 4]]
B = [[3, 4], [1, 2]]
print(det(A))
print(det(B))`,
			expected: "-2\n2\n",
		},
		{
			name: "det of 3x3 diagonal",
			code: `{{FUNC}}
A = [[2, 0, 0],
     [0, 3, 0],
     [0, 0, 4]]
print(det(A))`,
			expected: "24\n",
		},
	],
};
