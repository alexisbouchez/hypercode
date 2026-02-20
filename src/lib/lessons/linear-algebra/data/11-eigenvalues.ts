import type { Lesson } from "../../types";

export const eigenvalues: Lesson = {
	id: "eigenvalues",
	title: "Eigenvalues & Eigenvectors",
	chapterId: "systems",
	content: `## Eigenvalues and Eigenvectors

An **eigenvector** \`v\` of a matrix \`A\` is a vector that only gets **scaled** (not rotated) when multiplied by \`A\`:

\`\`\`
A · v = λ · v
\`\`\`

The scalar \`λ\` (lambda) is the **eigenvalue** — it tells you *how much* the eigenvector is stretched.

### The Characteristic Equation

Eigenvalues satisfy \`det(A - λI) = 0\`. For a 2×2 matrix:

\`\`\`
λ² - trace(A)·λ + det(A) = 0
\`\`\`

Using the quadratic formula:

\`\`\`python
import math

def eigenvalues_2x2(A):
    tr = A[0][0] + A[1][1]          # trace
    d = A[0][0]*A[1][1] - A[0][1]*A[1][0]   # determinant
    disc = tr**2 - 4*d
    l1 = (tr - math.sqrt(disc)) / 2
    l2 = (tr + math.sqrt(disc)) / 2
    return sorted([int(round(l1)), int(round(l2))])

A = [[3, 0], [0, 5]]
print(eigenvalues_2x2(A))   # [3, 5]
\`\`\`

### Diagonal Matrices

For diagonal matrices, the eigenvalues are just the diagonal entries.

### Why Eigenvalues Matter

- **PCA (Principal Component Analysis)** — eigenvectors of the covariance matrix give the principal components; eigenvalues give the variance explained
- **Google PageRank** — the web's link structure is a matrix; the PageRank vector is its dominant eigenvector
- **Stability analysis** — eigenvalues determine whether a dynamical system converges or diverges

### Your Task

Implement \`sorted_eigenvalues(A)\` that returns the eigenvalues of a 2×2 matrix \`A\` sorted in ascending order, as a list of integers.`,

	starterCode: `import math

def sorted_eigenvalues(A):
    # Return eigenvalues of 2×2 A sorted ascending, as integers
    pass

A = [[3, 0], [0, 5]]
print(sorted_eigenvalues(A))

B = [[1, 0], [0, 7]]
print(sorted_eigenvalues(B))
`,

	solution: `import math

def sorted_eigenvalues(A):
    tr = A[0][0] + A[1][1]
    d = A[0][0]*A[1][1] - A[0][1]*A[1][0]
    disc = tr**2 - 4*d
    l1 = (tr - math.sqrt(disc)) / 2
    l2 = (tr + math.sqrt(disc)) / 2
    return sorted([int(round(l1)), int(round(l2))])

A = [[3, 0], [0, 5]]
print(sorted_eigenvalues(A))

B = [[1, 0], [0, 7]]
print(sorted_eigenvalues(B))
`,

	tests: [
		{
			name: "diagonal matrices: eigenvalues are diagonal entries",
			expected: "[3, 5]\n[1, 7]\n",
		},
		{
			name: "identity has all eigenvalues = 1",
			code: `{{FUNC}}
print(sorted_eigenvalues([[1, 0], [0, 1]]))`,
			expected: "[1, 1]\n",
		},
		{
			name: "2x scaling matrix has eigenvalue 2",
			code: `{{FUNC}}
A = [[2, 0], [0, 2]]
print(sorted_eigenvalues(A))`,
			expected: "[2, 2]\n",
		},
		{
			name: "non-diagonal 2x2 matrix",
			code: `{{FUNC}}
A = [[4, 1], [2, 3]]
print(sorted_eigenvalues(A))`,
			expected: "[2, 5]\n",
		},
	],
};
