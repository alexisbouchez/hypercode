import type { Lesson } from "../../types";

export const conjugateGradientLesson: Lesson = {
	id: "conjugate-gradient",
	title: "Conjugate Gradient Method",
	chapterId: "applications",
	content: `## Conjugate Gradient Method

The **Conjugate Gradient (CG)** method solves symmetric positive definite systems $Ax = b$ iteratively — without factoring $A$. It is the algorithm of choice for large sparse systems.

### Key Idea

CG builds a sequence of search directions $\\mathbf{p}_0, \\mathbf{p}_1, \\ldots$ that are **A-conjugate** (mutually orthogonal in the energy inner product $\\langle \\mathbf{u}, \\mathbf{v} \\rangle_A = \\mathbf{u}^T A \\mathbf{v}$). Each step minimises the error over an expanding Krylov subspace.

### Algorithm

$$\\mathbf{x}_0 = \\mathbf{0}, \\quad \\mathbf{r}_0 = b, \\quad \\mathbf{p}_0 = b$$

For $k = 0, 1, 2, \\ldots$:

$$\\alpha_k = \\frac{\\mathbf{r}_k^T \\mathbf{r}_k}{\\mathbf{p}_k^T A \\mathbf{p}_k} \\quad (\\text{optimal step size})$$

$$\\mathbf{x} \\leftarrow \\mathbf{x} + \\alpha_k \\mathbf{p}_k, \\qquad \\mathbf{r} \\leftarrow \\mathbf{r} - \\alpha_k A \\mathbf{p}_k$$

$$\\beta_k = \\frac{\\mathbf{r}_{k+1}^T \\mathbf{r}_{k+1}}{\\mathbf{r}_k^T \\mathbf{r}_k} \\quad (\\text{correction factor}), \\qquad \\mathbf{p} \\leftarrow \\mathbf{r} + \\beta_k \\mathbf{p}_k$$

For an $n \\times n$ SPD system, CG converges in **at most $n$ iterations** (exactly, in exact arithmetic).

### Example

$$A = \\begin{pmatrix}4 & 1\\\\1 & 3\\end{pmatrix}, \\quad b = \\begin{pmatrix}1\\\\2\\end{pmatrix} \\quad \\Rightarrow \\quad x = \\begin{pmatrix}1/11\\\\7/11\\end{pmatrix} \\approx \\begin{pmatrix}0.0909\\\\0.6364\\end{pmatrix}$$

### Your Task

Implement \`conjugate_gradient(A, b)\` that solves $Ax = b$ for symmetric positive definite $A$.`,

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
