import type { Lesson } from "../../types";

export const deflationLesson: Lesson = {
	id: "deflation",
	title: "Eigenvalue Deflation",
	chapterId: "decompositions",
	content: `## Eigenvalue Deflation

Power iteration finds only the dominant eigenvalue. **Deflation** lets you find all eigenvalues one by one.

### Idea

After finding $(\\lambda_1, \\mathbf{v}_1)$, subtract its contribution from $A$:

$$A' = A - \\lambda_1 \\mathbf{v}_1 \\mathbf{v}_1^T$$

For symmetric $A$, $A'$ has the same eigenvectors as $A$ but eigenvalue $\\lambda_1$ becomes 0. Applying power iteration to $A'$ now converges to $\\lambda_2$.

### Why This Works

If $\\mathbf{u}$ is an eigenvector of $A$ with eigenvalue $\\lambda \\neq \\lambda_1$:

$$A'\\mathbf{u} = A\\mathbf{u} - \\lambda_1 (\\mathbf{v}_1 \\cdot \\mathbf{u})\\mathbf{v}_1 = \\lambda\\mathbf{u} - 0 = \\lambda\\mathbf{u}$$

(Since eigenvectors of a symmetric matrix are orthogonal: $\\mathbf{v}_1 \\cdot \\mathbf{u} = 0$.)

### Example

$$A = \\begin{pmatrix} 6 & 2 \\\\ 2 & 3 \\end{pmatrix}, \\quad \\text{eigenvalues: } 7, 2$$

After deflation by $\\lambda_1 = 7$, $\\mathbf{v}_1 = [0.8944,\\ 0.4472]$:

$$A' = \\begin{pmatrix} 0.4 & -0.8 \\\\ -0.8 & 1.6 \\end{pmatrix} \\quad \\Rightarrow \\quad \\text{dominant eigenvalue} = 2$$

### Your Task

Implement \`deflate(A, lam, v)\` that removes the $(\\lambda, \\mathbf{v})$ component from $A$, then use it with power iteration to extract the second eigenvalue.`,

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
