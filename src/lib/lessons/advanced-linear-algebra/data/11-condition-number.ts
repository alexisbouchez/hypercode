import type { Lesson } from "../../types";

export const conditionNumberLesson: Lesson = {
	id: "condition-number",
	title: "Condition Number",
	chapterId: "matrix-analysis",
	content: `## Condition Number

The **condition number** $\\kappa(A)$ measures how sensitive a linear system $Ax = b$ is to small perturbations:

$$\\kappa(A) = \\frac{\\lambda_{\\max}}{\\lambda_{\\min}} \\quad (\\text{for symmetric positive definite matrices})$$

A large condition number means the system is **ill-conditioned**: tiny errors in $b$ can cause enormous errors in $x$.

### 2×2 Eigenvalues via Trace and Determinant

For a 2×2 symmetric matrix, eigenvalues are:

$$\\lambda = \\frac{\\text{tr} \\pm \\sqrt{\\text{tr}^2 - 4 \\cdot \\det}}{2}$$

where $\\text{tr} = A_{00} + A_{11}$ and $\\det = A_{00} A_{11} - A_{01} A_{10}$.

### Interpretation

| $\\kappa(A)$ | Meaning |
|------|---------|
| $\\approx 1$ | Well-conditioned; safe to solve |
| $\\approx 10^6$ | Lose about 6 digits of precision |
| $\\approx 10^{16}$ | Essentially singular on 64-bit floats |

### Examples

$$A = \\begin{pmatrix}3&1\\\\1&3\\end{pmatrix}: \\; \\text{tr}=6,\\ \\det=8,\\ \\lambda = \\tfrac{6\\pm2}{2} = 4, 2 \\;\\ \\Rightarrow \\;\\ \\kappa = 2.0$$

$$B = \\begin{pmatrix}2&1\\\\1&2\\end{pmatrix}: \\; \\text{tr}=4,\\ \\det=3,\\ \\lambda = 3, 1 \\;\\ \\Rightarrow \\;\\ \\kappa = 3.0$$

$$C = I: \\; \\lambda = 1, 1 \\;\\ \\Rightarrow \\;\\ \\kappa = 1.0$$

### Your Task

Implement \`condition_number_2x2(A)\` that computes $\\kappa = \\lambda_{\\max} / \\lambda_{\\min}$ for a 2×2 symmetric positive definite matrix using the trace-determinant formula.`,

	starterCode: `import math

def condition_number_2x2(A):
    tr = A[0][0] + A[1][1]
    det = A[0][0] * A[1][1] - A[0][1] * A[1][0]
    disc = math.sqrt(tr * tr - 4 * det)
    lam_max = (tr + disc) / 2
    lam_min = (tr - disc) / 2
    return lam_max / lam_min

print(f"{condition_number_2x2([[3, 1], [1, 3]]):.4f}")
print(f"{condition_number_2x2([[1, 0], [0, 1]]):.4f}")
`,

	solution: `import math

def condition_number_2x2(A):
    tr = A[0][0] + A[1][1]
    det = A[0][0] * A[1][1] - A[0][1] * A[1][0]
    disc = math.sqrt(tr * tr - 4 * det)
    lam_max = (tr + disc) / 2
    lam_min = (tr - disc) / 2
    return lam_max / lam_min

print(f"{condition_number_2x2([[3, 1], [1, 3]]):.4f}")
print(f"{condition_number_2x2([[1, 0], [0, 1]]):.4f}")
`,

	tests: [
		{
			name: "condition_number_2x2([[3,1],[1,3]]) = 2",
			code: `{{FUNC}}
print(f"{condition_number_2x2([[3, 1], [1, 3]]):.4f}")`,
			expected: "2.0000\n",
		},
		{
			name: "condition_number_2x2([[1,0],[0,1]]) = 1",
			code: `{{FUNC}}
print(f"{condition_number_2x2([[1, 0], [0, 1]]):.4f}")`,
			expected: "1.0000\n",
		},
		{
			name: "condition_number_2x2([[2,0],[0,8]]) = 4",
			code: `{{FUNC}}
print(f"{condition_number_2x2([[2, 0], [0, 8]]):.4f}")`,
			expected: "4.0000\n",
		},
		{
			name: "condition_number_2x2([[2,1],[1,2]]) = 3",
			code: `{{FUNC}}
print(f"{condition_number_2x2([[2, 1], [1, 2]]):.4f}")`,
			expected: "3.0000\n",
		},
	],
};
