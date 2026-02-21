import type { Lesson } from "../../types";

export const pca2d: Lesson = {
	id: "pca-2d",
	title: "PCA (2D)",
	chapterId: "unsupervised",
	content: `## Principal Component Analysis (2D)

**PCA** finds the directions of maximum variance in the data. It is used for dimensionality reduction, visualisation, and noise removal.

### Step 1 — Center the Data

Subtract the column mean from each feature so the data has zero mean:

$$\\tilde{x}_{ij} = x_{ij} - \\bar{x}_j$$

### Step 2 — Covariance Matrix (2D)

For centered 2D data with $n$ points:

$$\\Sigma = \\frac{1}{n-1} \\begin{bmatrix} \\sum \\tilde{x}_1^2 & \\sum \\tilde{x}_1 \\tilde{x}_2 \\\\ \\sum \\tilde{x}_1 \\tilde{x}_2 & \\sum \\tilde{x}_2^2 \\end{bmatrix}$$

### Step 3 — Explained Variance Ratio

Given eigenvalues $\\lambda_1 \\geq \\lambda_2 \\geq \\ldots$:

$$\\text{EVR}_i = \\frac{\\lambda_i}{\\sum_j \\lambda_j}$$

If the first component's EVR is close to 1, most variance lives along a single direction.

### Your Task

Implement:
- \`center(X)\` → subtract column means from each row
- \`covariance_2d(X_centered)\` → $2 \\times 2$ covariance matrix as a list of lists
- \`explained_variance_ratio(eigenvalues)\` → list of ratios`,

	starterCode: `def center(X):
    # Subtract the mean of each column
    return X

def covariance_2d(X_centered):
    # [[var_x, cov_xy], [cov_xy, var_y]] / (n-1)
    return [[0.0, 0.0], [0.0, 0.0]]

def explained_variance_ratio(eigenvalues):
    # Each eigenvalue divided by their sum
    return []

X = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
Xc = center(X)
print(Xc)                                                   # [[-2,-2],[0,0],[2,2]]
cov = covariance_2d(Xc)
print([[round(v, 4) for v in row] for row in cov])          # [[4.0,4.0],[4.0,4.0]]
print(explained_variance_ratio([3.0, 1.0]))                 # [0.75, 0.25]
`,

	solution: `def center(X):
    n = len(X)
    dim = len(X[0])
    means = [sum(X[i][d] for i in range(n)) / n for d in range(dim)]
    return [[X[i][d] - means[d] for d in range(dim)] for i in range(n)]

def covariance_2d(X_centered):
    n = len(X_centered)
    var_x = sum(x[0] ** 2 for x in X_centered) / (n - 1)
    var_y = sum(x[1] ** 2 for x in X_centered) / (n - 1)
    cov_xy = sum(x[0] * x[1] for x in X_centered) / (n - 1)
    return [[var_x, cov_xy], [cov_xy, var_y]]

def explained_variance_ratio(eigenvalues):
    total = sum(eigenvalues)
    return [e / total for e in eigenvalues]

X = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
Xc = center(X)
print(Xc)
cov = covariance_2d(Xc)
print([[round(v, 4) for v in row] for row in cov])
print(explained_variance_ratio([3.0, 1.0]))
`,

	tests: [
		{
			name: "center, covariance, and EVR",
			expected: "[[-2.0, -2.0], [0.0, 0.0], [2.0, 2.0]]\n[[4.0, 4.0], [4.0, 4.0]]\n[0.75, 0.25]\n",
		},
		{
			name: "explained_variance_ratio([2.0, 2.0]) = [0.5, 0.5]",
			code: `{{FUNC}}
print(explained_variance_ratio([2.0, 2.0]))`,
			expected: "[0.5, 0.5]\n",
		},
		{
			name: "center single-point dataset gives zeros",
			code: `{{FUNC}}
print(center([[3.0, 7.0]]))`,
			expected: "[[0.0, 0.0]]\n",
		},
		{
			name: "covariance_2d of anti-correlated data",
			code: `{{FUNC}}
X = [[-1.0, 1.0], [1.0, -1.0]]
cov = covariance_2d(X)
print([[round(v, 4) for v in row] for row in cov])`,
			expected: "[[2.0, -2.0], [-2.0, 2.0]]\n",
		},
	],
};
