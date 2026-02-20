import type { Lesson } from "../../types";

export const powerIterationLesson: Lesson = {
	id: "power-iteration",
	title: "Power Iteration",
	chapterId: "decompositions",
	content: `## Power Iteration

Power iteration finds the **dominant eigenvalue** (largest in magnitude) and its eigenvector by repeated matrix-vector multiplication:

\`\`\`
v₀ = random unit vector
vₖ₊₁ = A vₖ / ‖A vₖ‖
λ = vᵀ A v   (Rayleigh quotient)
\`\`\`

### Why It Works

Expand v₀ in the eigenbasis: v₀ = Σ cᵢ uᵢ. After k multiplications:

\`\`\`
Aᵏ v₀ = Σ cᵢ λᵢᵏ uᵢ  ≈  c₁ λ₁ᵏ u₁  (since λ₁ > |λ₂| ≥ … )
\`\`\`

The dominant component grows fastest and eventually dominates.

### Convergence

The rate is |λ₂/λ₁|. If the second eigenvalue is small relative to the first, convergence is fast.

### Example

\`\`\`
A = [[4, 1],   eigenvalues: 5, 2
     [2, 3]]   dominant eigenvector: [0.7071, 0.7071]
\`\`\`

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
