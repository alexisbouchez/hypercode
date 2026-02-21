import type { Lesson } from "../../types";

export const powerIterationLesson: Lesson = {
	id: "power-iteration",
	title: "Power Iteration",
	chapterId: "decompositions",
	content: `## Power Iteration

Power iteration finds the **dominant eigenvalue** (largest in magnitude) and its eigenvector by repeated matrix-vector multiplication:

$$\\mathbf{v}_{0} = \\text{random unit vector}, \\qquad \\mathbf{v}_{k+1} = \\frac{A\\mathbf{v}_{k}}{\\|A\\mathbf{v}_{k}\\|}, \\qquad \\lambda = \\mathbf{v}^T A \\mathbf{v} \\; (\\text{Rayleigh quotient})$$

### Why It Works

Expand $\\mathbf{v}_{0}$ in the eigenbasis: $\\mathbf{v}_{0} = \\sum_i c_i \\mathbf{u}_{i}$. After $k$ multiplications:

$$A^k \\mathbf{v}_{0} = \\sum_i c_i \\lambda_i^k \\mathbf{u}_{i} \\approx c_1 \\lambda_1^k \\mathbf{u}_{1} \\quad (\\text{since } |\\lambda_1| > |\\lambda_2| \\geq \\ldots)$$

The dominant component grows fastest and eventually dominates.

### Convergence

The rate is $|\\lambda_2 / \\lambda_1|$. If the second eigenvalue is small relative to the first, convergence is fast.

### Example

$$A = \\begin{pmatrix} 4 & 1 \\\\ 2 & 3 \\end{pmatrix}, \\quad \\text{eigenvalues: } 5, 2, \\quad \\text{dominant eigenvector: } [0.7071,\\ 0.7071]$$

### Your Task

Implement \`power_iteration(A, iterations=100)\` returning \`(eigenvalue, eigenvector)\`.`,

	starterCode: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def matvec(A, v):
    return [sum(A[i][j] * v[j] for j in range(len(v))) for i in range(len(A))]

def normalize(v):
    n = math.sqrt(dot(v, v))
    return [x / n for x in v]

def power_iteration(A, iterations=100):
    n = len(A)
    v = normalize([1.0] * n)
    eigenvalue = 0.0
    for _ in range(iterations):
        w = matvec(A, v)
        eigenvalue = dot(w, v)
        v = normalize(w)
    return eigenvalue, v

A = [[4, 1], [2, 3]]
eigenvalue, eigenvec = power_iteration(A)
print(f"{eigenvalue:.4f}")
print(" ".join(f"{x:.4f}" for x in eigenvec))
`,

	solution: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def matvec(A, v):
    return [sum(A[i][j] * v[j] for j in range(len(v))) for i in range(len(A))]

def normalize(v):
    n = math.sqrt(dot(v, v))
    return [x / n for x in v]

def power_iteration(A, iterations=100):
    n = len(A)
    v = normalize([1.0] * n)
    eigenvalue = 0.0
    for _ in range(iterations):
        w = matvec(A, v)
        eigenvalue = dot(w, v)
        v = normalize(w)
    return eigenvalue, v

A = [[4, 1], [2, 3]]
eigenvalue, eigenvec = power_iteration(A)
print(f"{eigenvalue:.4f}")
print(" ".join(f"{x:.4f}" for x in eigenvec))
`,

	tests: [
		{
			name: "dominant eigenvalue of [[4,1],[2,3]] = 5",
			code: `{{FUNC}}
lam, v = power_iteration([[4, 1], [2, 3]])
print(f"{lam:.4f}")`,
			expected: "5.0000\n",
		},
		{
			name: "dominant eigenvector of [[4,1],[2,3]]",
			code: `{{FUNC}}
lam, v = power_iteration([[4, 1], [2, 3]])
print(" ".join(f"{x:.4f}" for x in v))`,
			expected: "0.7071 0.7071\n",
		},
		{
			name: "dominant eigenvalue of [[3,0],[0,2]] = 3",
			code: `{{FUNC}}
lam, v = power_iteration([[3, 0], [0, 2]])
print(f"{lam:.4f}")`,
			expected: "3.0000\n",
		},
		{
			name: "dominant eigenvalue of [[6,2],[2,3]] = 7",
			code: `{{FUNC}}
lam, v = power_iteration([[6, 2], [2, 3]])
print(f"{lam:.4f}")`,
			expected: "7.0000\n",
		},
	],
};
