import type { Lesson } from "../../types";

export const pcaLesson: Lesson = {
	id: "pca",
	title: "Principal Component Analysis",
	chapterId: "applications",
	content: `## Principal Component Analysis (PCA)

PCA finds the directions of maximum variance in data — the **principal components**. The first PC is the direction along which the data varies most.

### Algorithm

1. **Centre** the data by subtracting the mean
2. **Compute the covariance matrix** C = (1/n) Xᵀ X
3. **Find the dominant eigenvector** of C via power iteration

The dominant eigenvector is the first principal component.

### Example

Data: (2,1), (4,2), (6,3), (8,4) — perfectly collinear along y = x/2

After centring, the covariance matrix is:
\`\`\`
C = [[5.0, 2.5],
     [2.5, 1.25]]
\`\`\`

The first PC (dominant eigenvector) points along [2, 1] / √5:
\`\`\`
PC₁ = [0.8944, 0.4472]
\`\`\`

### Your Task

Implement \`pca_first_component(data)\` that returns the first principal component (unit vector) of a list of 2D points.`,

	starterCode: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def matvec(A, v):
    return [sum(A[i][j] * v[j] for j in range(len(v))) for i in range(len(A))]

def normalize(v):
    n = math.sqrt(dot(v, v))
    return [x / n for x in v]

def power_iteration(A, iterations=200):
    n = len(A)
    v = normalize([1.0] * n)
    eigenvalue = 0.0
    for _ in range(iterations):
        w = matvec(A, v)
        eigenvalue = dot(w, v)
        v = normalize(w)
    return eigenvalue, v

def pca_first_component(data):
    n = len(data)
    mx = sum(p[0] for p in data) / n
    my = sum(p[1] for p in data) / n
    centered = [[p[0] - mx, p[1] - my] for p in data]
    # Compute 2x2 covariance matrix
    cxx = sum(c[0]**2 for c in centered) / n
    cxy = sum(c[0]*c[1] for c in centered) / n
    cyy = sum(c[1]**2 for c in centered) / n
    cov = [[cxx, cxy], [cxy, cyy]]
    _, pc = power_iteration(cov)
    if pc[0] < 0:
        pc = [-x for x in pc]
    return pc

data = [[2, 1], [4, 2], [6, 3], [8, 4]]
pc = pca_first_component(data)
print(" ".join(f"{x:.4f}" for x in pc))
`,

	solution: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def matvec(A, v):
    return [sum(A[i][j] * v[j] for j in range(len(v))) for i in range(len(A))]

def normalize(v):
    n = math.sqrt(dot(v, v))
    return [x / n for x in v]

def power_iteration(A, iterations=200):
    n = len(A)
    v = normalize([1.0] * n)
    eigenvalue = 0.0
    for _ in range(iterations):
        w = matvec(A, v)
        eigenvalue = dot(w, v)
        v = normalize(w)
    return eigenvalue, v

def pca_first_component(data):
    n = len(data)
    mx = sum(p[0] for p in data) / n
    my = sum(p[1] for p in data) / n
    centered = [[p[0] - mx, p[1] - my] for p in data]
    cxx = sum(c[0]**2 for c in centered) / n
    cxy = sum(c[0]*c[1] for c in centered) / n
    cyy = sum(c[1]**2 for c in centered) / n
    cov = [[cxx, cxy], [cxy, cyy]]
    _, pc = power_iteration(cov)
    if pc[0] < 0:
        pc = [-x for x in pc]
    return pc

data = [[2, 1], [4, 2], [6, 3], [8, 4]]
pc = pca_first_component(data)
print(" ".join(f"{x:.4f}" for x in pc))
`,

	tests: [
		{
			name: "PCA of collinear data (2,1),(4,2),(6,3),(8,4) → [0.8944, 0.4472]",
			code: `{{FUNC}}
data = [[2, 1], [4, 2], [6, 3], [8, 4]]
pc = pca_first_component(data)
print(" ".join(f"{x:.4f}" for x in pc))`,
			expected: "0.8944 0.4472\n",
		},
		{
			name: "PCA of horizontally spread data → PC along [1,0]",
			code: `{{FUNC}}
data = [[1, 0], [2, 0], [3, 0], [4, 0]]
pc = pca_first_component(data)
print(" ".join(f"{x:.4f}" for x in pc))`,
			expected: "1.0000 0.0000\n",
		},
		{
			name: "PCA of diagonally spread data → PC along [0.7071, 0.7071]",
			code: `{{FUNC}}
data = [[-2, -2], [-1, -1], [1, 1], [2, 2]]
pc = pca_first_component(data)
print(" ".join(f"{x:.4f}" for x in pc))`,
			expected: "0.7071 0.7071\n",
		},
		{
			name: "PCA first component is a unit vector",
			code: `{{FUNC}}
import math
data = [[1, 3], [2, 5], [3, 4], [4, 6]]
pc = pca_first_component(data)
norm = math.sqrt(sum(x**2 for x in pc))
print(f"{norm:.4f}")`,
			expected: "1.0000\n",
		},
	],
};
