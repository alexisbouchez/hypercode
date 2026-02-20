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
import numpy as np

A = np.array([[3, 0],
              [0, 2]])

print(np.linalg.det(A))  # 6.0  — scales area by 6
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
S = np.array([[1, 2],
              [2, 4]])  # row 2 = 2 × row 1

print(int(round(np.linalg.det(S))))  # 0
\`\`\`

### Floating Point Note

\`np.linalg.det\` returns a float. Always round to an integer when you expect an exact integer result.

### Your Task

Implement \`det(A)\` that returns the determinant as an integer (rounded).`,

	starterCode: `import numpy as np

def det(A):
    # Return the determinant as a rounded integer
    pass

print(det(np.array([[3, 0], [0, 2]])))
print(det(np.array([[1, 2], [3, 4]])))
print(det(np.array([[1, 2], [2, 4]])))
`,

	solution: `import numpy as np

def det(A):
    return int(round(np.linalg.det(A)))

print(det(np.array([[3, 0], [0, 2]])))
print(det(np.array([[1, 2], [3, 4]])))
print(det(np.array([[1, 2], [2, 4]])))
`,

	tests: [
		{
			name: "diagonal, 2x2, singular",
			expected: "6\n-2\n0\n",
		},
		{
			name: "identity has det 1",
			code: `{{FUNC}}
print(det(np.eye(3, dtype=int)))`,
			expected: "1\n",
		},
		{
			name: "swap rows negates determinant",
			code: `{{FUNC}}
A = np.array([[1, 2], [3, 4]])
B = np.array([[3, 4], [1, 2]])
print(det(A))
print(det(B))`,
			expected: "-2\n2\n",
		},
		{
			name: "det of 3x3",
			code: `{{FUNC}}
A = np.array([[2, 0, 0],
              [0, 3, 0],
              [0, 0, 4]])
print(det(A))`,
			expected: "24\n",
		},
	],
};
