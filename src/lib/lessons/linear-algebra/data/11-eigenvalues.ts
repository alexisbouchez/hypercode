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

\`\`\`python
import numpy as np

A = np.array([[3, 0],
              [0, 5]])

vals, vecs = np.linalg.eig(A)
print(np.sort(vals.real).astype(int))  # [3 5]
\`\`\`

### Diagonal Matrices

For diagonal matrices, the eigenvalues are just the diagonal entries — and the eigenvectors are the standard basis vectors.

### Why Eigenvalues Matter

- **PCA (Principal Component Analysis)** — eigenvectors of the covariance matrix give the principal components; eigenvalues give the variance explained
- **Google PageRank** — the web's link structure is a matrix; the PageRank vector is its dominant eigenvector
- **Stability analysis** — eigenvalues determine whether a dynamical system converges or diverges
- **Graph Laplacian** — eigenvalues reveal cluster structure (spectral clustering)

### The Characteristic Equation

Eigenvalues satisfy \`det(A - λI) = 0\`. For a 2×2 matrix:

\`\`\`
λ² - trace(A)·λ + det(A) = 0
\`\`\`

### Your Task

Implement \`sorted_eigenvalues(A)\` that returns the eigenvalues of \`A\` sorted in ascending order, as integers.`,

	starterCode: `import numpy as np

def sorted_eigenvalues(A):
    # Return eigenvalues of A sorted ascending, as integers
    pass

A = np.array([[3, 0], [0, 5]])
print(sorted_eigenvalues(A))

B = np.array([[1, 0], [0, 7]])
print(sorted_eigenvalues(B))
`,

	solution: `import numpy as np

def sorted_eigenvalues(A):
    vals, _ = np.linalg.eig(A)
    return np.sort(vals.real).astype(int)

A = np.array([[3, 0], [0, 5]])
print(sorted_eigenvalues(A))

B = np.array([[1, 0], [0, 7]])
print(sorted_eigenvalues(B))
`,

	tests: [
		{
			name: "diagonal matrices: eigenvalues are diagonal entries",
			expected: "[3 5]\n[1 7]\n",
		},
		{
			name: "identity has all eigenvalues = 1",
			code: `{{FUNC}}
print(sorted_eigenvalues(np.eye(3, dtype=int)))`,
			expected: "[1 1 1]\n",
		},
		{
			name: "2x scaling matrix has eigenvalue 2",
			code: `{{FUNC}}
A = np.array([[2, 0], [0, 2]])
print(sorted_eigenvalues(A))`,
			expected: "[2 2]\n",
		},
		{
			name: "3x3 diagonal",
			code: `{{FUNC}}
A = np.array([[4, 0, 0],
              [0, 1, 0],
              [0, 0, 6]])
print(sorted_eigenvalues(A))`,
			expected: "[1 4 6]\n",
		},
	],
};
