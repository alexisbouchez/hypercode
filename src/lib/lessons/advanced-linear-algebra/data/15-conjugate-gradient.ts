import type { Lesson } from "../../types";

export const conjugateGradientLesson: Lesson = {
	id: "conjugate-gradient",
	title: "Conjugate Gradient Method",
	chapterId: "applications",
	content: `## Conjugate Gradient Method

The **Conjugate Gradient (CG)** method solves symmetric positive definite systems Ax = b iteratively — without factoring A. It is the algorithm of choice for large sparse systems.

### Key Idea

CG builds a sequence of search directions p₀, p₁, … that are **A-conjugate** (mutually orthogonal in the energy inner product ⟨u,v⟩_A = uᵀAv). Each step minimises the error over an expanding Krylov subspace.

### Algorithm

\`\`\`
x₀ = 0,  r₀ = b,  p₀ = b

for k = 0, 1, 2, …:
    α  = rₖᵀrₖ / (pₖᵀ A pₖ)   ← optimal step size
    x  = x + α pₖ
    r  = r − α A pₖ
    β  = rₖ₊₁ᵀrₖ₊₁ / rₖᵀrₖ    ← correction factor
    p  = r + β pₖ
\`\`\`

For an n×n SPD system, CG converges in **at most n iterations** (exactly, in exact arithmetic).

### Example

\`\`\`
A = [[4, 1],   b = [1, 2]   →   x = [1/11, 7/11] ≈ [0.0909, 0.6364]
     [1, 3]]
\`\`\`

### Your Task

Implement \`conjugate_gradient(A, b)\` that solves Ax = b for symmetric positive definite A.`,

	starterCode: `def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def matvec(A, v):
    return [sum(A[i][j] * v[j] for j in range(len(v))) for i in range(len(A))]

def vec_axpy(a, x, b, y):
    """Compute a*x + b*y elementwise"""
    return [a * xi + b * yi for xi, yi in zip(x, y)]

def conjugate_gradient(A, b, tol=1e-10):
    n = len(b)
    x = [0.0] * n
    r = list(b)
    p = list(r)
    rs_old = dot(r, r)
    for _ in range(n * 10):
        Ap = matvec(A, p)
        alpha = rs_old / dot(p, Ap)
        x = vec_axpy(1.0, x, alpha, p)
        r = vec_axpy(1.0, r, -alpha, Ap)
        rs_new = dot(r, r)
        if rs_new < tol:
            break
        beta = rs_new / rs_old
        p = vec_axpy(1.0, r, beta, p)
        rs_old = rs_new
    return x

A = [[4, 1], [1, 3]]
b = [1, 2]
x = conjugate_gradient(A, b)
print(" ".join(f"{v:.4f}" for v in x))
`,

	solution: `def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def matvec(A, v):
    return [sum(A[i][j] * v[j] for j in range(len(v))) for i in range(len(A))]

def vec_axpy(a, x, b, y):
    return [a * xi + b * yi for xi, yi in zip(x, y)]

def conjugate_gradient(A, b, tol=1e-10):
    n = len(b)
    x = [0.0] * n
    r = list(b)
    p = list(r)
    rs_old = dot(r, r)
    for _ in range(n * 10):
        Ap = matvec(A, p)
        alpha = rs_old / dot(p, Ap)
        x = vec_axpy(1.0, x, alpha, p)
        r = vec_axpy(1.0, r, -alpha, Ap)
        rs_new = dot(r, r)
        if rs_new < tol:
            break
        beta = rs_new / rs_old
        p = vec_axpy(1.0, r, beta, p)
        rs_old = rs_new
    return x

A = [[4, 1], [1, 3]]
b = [1, 2]
x = conjugate_gradient(A, b)
print(" ".join(f"{v:.4f}" for v in x))
`,

	tests: [
		{
			name: "solve [[4,1],[1,3]]x=[1,2] → x=[1/11, 7/11]",
			code: `{{FUNC}}
x = conjugate_gradient([[4, 1], [1, 3]], [1, 2])
print(" ".join(f"{v:.4f}" for v in x))`,
			expected: "0.0909 0.6364\n",
		},
		{
			name: "solve identity system Ix=b → x=b",
			code: `{{FUNC}}
x = conjugate_gradient([[1, 0], [0, 1]], [3, 7])
print(" ".join(f"{v:.4f}" for v in x))`,
			expected: "3.0000 7.0000\n",
		},
		{
			name: "solve 3×3 diagonal system",
			code: `{{FUNC}}
A = [[2, 0, 0], [0, 3, 0], [0, 0, 4]]
b = [4, 9, 8]
x = conjugate_gradient(A, b)
print(" ".join(f"{v:.4f}" for v in x))`,
			expected: "2.0000 3.0000 2.0000\n",
		},
		{
			name: "solve [[3,1],[1,2]]x=[5,4] → x=[6/5, 7/5]",
			code: `{{FUNC}}
x = conjugate_gradient([[3, 1], [1, 2]], [5, 4])
print(" ".join(f"{v:.4f}" for v in x))`,
			expected: "1.2000 1.4000\n",
		},
	],
};
