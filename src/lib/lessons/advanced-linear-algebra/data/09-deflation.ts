import type { Lesson } from "../../types";

export const deflationLesson: Lesson = {
	id: "deflation",
	title: "Eigenvalue Deflation",
	chapterId: "decompositions",
	content: `## Eigenvalue Deflation

Power iteration finds only the dominant eigenvalue. **Deflation** lets you find all eigenvalues one by one.

### Idea

After finding (λ₁, v₁), subtract its contribution from A:

\`\`\`
A' = A − λ₁ v₁ v₁ᵀ
\`\`\`

For symmetric A, A' has the same eigenvectors as A but eigenvalue λ₁ becomes 0. Applying power iteration to A' now converges to λ₂.

### Why This Works

If u is an eigenvector of A with eigenvalue λ ≠ λ₁:

\`\`\`
A'u = Au − λ₁(v₁·u)v₁ = λu − 0 = λu
\`\`\`

(Since eigenvectors of a symmetric matrix are orthogonal: v₁·u = 0.)

### Example

\`\`\`
A = [[6, 2],   eigenvalues: 7, 2
     [2, 3]]

After deflation by λ₁=7, v₁=[0.8944, 0.4472]:
A' = [[0.4, -0.8],   →   dominant eigenvalue = 2
      [-0.8, 1.6]]
\`\`\`

### Your Task

Implement \`deflate(A, lam, v)\` that removes the (λ, v) component from A, then use it with power iteration to extract the second eigenvalue.`,

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

def deflate(A, lam, v):
    # A' = A - lam * v * v^T
    n = len(A)
    return [[A[i][j] - lam * v[i] * v[j] for j in range(n)] for i in range(n)]

A = [[6, 2], [2, 3]]
lam1, v1 = power_iteration(A)
A2 = deflate(A, lam1, v1)
lam2, v2 = power_iteration(A2)
print(f"{lam1:.4f}")
print(f"{lam2:.4f}")
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

def deflate(A, lam, v):
    n = len(A)
    return [[A[i][j] - lam * v[i] * v[j] for j in range(n)] for i in range(n)]

A = [[6, 2], [2, 3]]
lam1, v1 = power_iteration(A)
A2 = deflate(A, lam1, v1)
lam2, v2 = power_iteration(A2)
print(f"{lam1:.4f}")
print(f"{lam2:.4f}")
`,

	tests: [
		{
			name: "first eigenvalue of [[6,2],[2,3]] = 7",
			code: `{{FUNC}}
A = [[6, 2], [2, 3]]
lam1, v1 = power_iteration(A)
print(f"{lam1:.4f}")`,
			expected: "7.0000\n",
		},
		{
			name: "second eigenvalue of [[6,2],[2,3]] via deflation = 2",
			code: `{{FUNC}}
A = [[6, 2], [2, 3]]
lam1, v1 = power_iteration(A)
A2 = deflate(A, lam1, v1)
lam2, v2 = power_iteration(A2)
print(f"{lam2:.4f}")`,
			expected: "2.0000\n",
		},
		{
			name: "both eigenvalues of [[7,2],[2,4]] (eigenvalues 8,3)",
			code: `{{FUNC}}
A = [[7, 2], [2, 4]]
lam1, v1 = power_iteration(A)
A2 = deflate(A, lam1, v1)
lam2, v2 = power_iteration(A2)
eigs = sorted([lam1, lam2], reverse=True)
print(f"{eigs[0]:.4f}")
print(f"{eigs[1]:.4f}")`,
			expected: "8.0000\n3.0000\n",
		},
		{
			name: "deflation preserves other eigenvalue",
			code: `{{FUNC}}
A = [[4, 0], [0, 1]]
lam1, v1 = power_iteration(A)
A2 = deflate(A, lam1, v1)
lam2, v2 = power_iteration(A2)
print(f"{lam1:.4f}")
print(f"{lam2:.4f}")`,
			expected: "4.0000\n1.0000\n",
		},
	],
};
