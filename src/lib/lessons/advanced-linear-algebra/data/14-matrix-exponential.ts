import type { Lesson } from "../../types";

export const matrixExponentialLesson: Lesson = {
	id: "matrix-exponential",
	title: "Matrix Exponential",
	chapterId: "applications",
	content: `## Matrix Exponential

The **matrix exponential** eᴬ extends the scalar exponential to matrices via the Taylor series:

\`\`\`
eᴬ = I + A + A²/2! + A³/3! + A⁴/4! + …
\`\`\`

### Applications

- **Differential equations**: the solution to ẋ = Ax with x(0) = x₀ is x(t) = e^(tA) x₀
- **Control theory**: state transition matrices
- **Quantum mechanics**: time evolution operator

### Algorithm

Accumulate terms until convergence:

\`\`\`
result = I
term = I
for k = 1, 2, 3, …:
    term = term × A / k
    result += term
\`\`\`

### Example: Rotation Generator

The matrix A = [[0, 1], [-1, 0]] is the generator of 2D rotation. Its exponential is exactly the rotation matrix:

\`\`\`
e^A = [[cos(1), sin(1)],   ≈ [[0.5403,  0.8415],
       [-sin(1), cos(1)]]      [-0.8415, 0.5403]]
\`\`\`

### Your Task

Implement \`mat_exp(A, terms=20)\` using the Taylor series. Use matrix multiplication to compute successive powers of A.`,

	starterCode: `def matmul(A, B):
    n = len(A)
    return [[sum(A[i][k] * B[k][j] for k in range(n))
             for j in range(n)] for i in range(n)]

def mat_scale(A, s):
    return [[x * s for x in row] for row in A]

def mat_add(A, B):
    return [[A[i][j] + B[i][j] for j in range(len(A[0]))]
            for i in range(len(A))]

def mat_exp(A, terms=20):
    n = len(A)
    result = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
    term   = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
    for k in range(1, terms):
        term = mat_scale(matmul(term, A), 1.0 / k)
        result = mat_add(result, term)
    return result

A = [[0, 1], [-1, 0]]
E = mat_exp(A)
for row in E:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	solution: `def matmul(A, B):
    n = len(A)
    return [[sum(A[i][k] * B[k][j] for k in range(n))
             for j in range(n)] for i in range(n)]

def mat_scale(A, s):
    return [[x * s for x in row] for row in A]

def mat_add(A, B):
    return [[A[i][j] + B[i][j] for j in range(len(A[0]))]
            for i in range(len(A))]

def mat_exp(A, terms=20):
    n = len(A)
    result = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
    term   = [[1.0 if i == j else 0.0 for j in range(n)] for i in range(n)]
    for k in range(1, terms):
        term = mat_scale(matmul(term, A), 1.0 / k)
        result = mat_add(result, term)
    return result

A = [[0, 1], [-1, 0]]
E = mat_exp(A)
for row in E:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	tests: [
		{
			name: "mat_exp([[0,1],[-1,0]]) = rotation by 1 rad — top row",
			code: `{{FUNC}}
E = mat_exp([[0, 1], [-1, 0]])
print(" ".join(f"{x:.4f}" for x in E[0]))`,
			expected: "0.5403 0.8415\n",
		},
		{
			name: "mat_exp([[0,1],[-1,0]]) — bottom row",
			code: `{{FUNC}}
E = mat_exp([[0, 1], [-1, 0]])
print(" ".join(f"{x:.4f}" for x in E[1]))`,
			expected: "-0.8415 0.5403\n",
		},
		{
			name: "mat_exp of zero matrix = identity",
			code: `{{FUNC}}
E = mat_exp([[0, 0], [0, 0]])
for row in E:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "1.0000 0.0000\n0.0000 1.0000\n",
		},
		{
			name: "mat_exp([[1,0],[0,2]]) diagonal = [e, e²]",
			code: `{{FUNC}}
import math
E = mat_exp([[1, 0], [0, 2]])
print(f"{E[0][0]:.4f}")
print(f"{E[1][1]:.4f}")`,
			expected: "2.7183\n7.3891\n",
		},
	],
};
