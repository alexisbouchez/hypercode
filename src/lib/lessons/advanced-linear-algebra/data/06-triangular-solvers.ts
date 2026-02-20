import type { Lesson } from "../../types";

export const triangularSolversLesson: Lesson = {
	id: "triangular-solvers",
	title: "Triangular System Solvers",
	chapterId: "decompositions",
	content: `## Triangular System Solvers

After LU decomposition, solving Ax = b reduces to two cheap triangular solves:

\`\`\`
A = LU  →  LUx = b
Step 1: Ly = b  (forward substitution)
Step 2: Ux = y  (backward substitution)
\`\`\`

### Forward Substitution (Ly = b)

L is lower triangular, so solve top-to-bottom:

\`\`\`
y[0] = b[0] / L[0][0]
y[i] = (b[i] - Σⱼ<ᵢ L[i][j] y[j]) / L[i][i]
\`\`\`

### Backward Substitution (Ux = y)

U is upper triangular, so solve bottom-to-top:

\`\`\`
x[n-1] = y[n-1] / U[n-1][n-1]
x[i]   = (y[i] - Σⱼ>ᵢ U[i][j] x[j]) / U[i][i]
\`\`\`

### Example

\`\`\`
L = [[1,0,0],[2,1,0],[4,3,1]]   b = [1, 3, 9]
U = [[2,1,1],[0,1,1],[0,0,2]]

Ly = b  →  y = [1.0, 1.0, 2.0]
Ux = y  →  x = [0.0, 0.0, 1.0]
\`\`\`

### Your Task

Implement \`forward_sub(L, b)\` and \`backward_sub(U, b)\`, then combine them in \`solve_lu(L, U, b)\`.`,

	starterCode: `def forward_sub(L, b):
    n = len(b)
    x = [0.0] * n
    for i in range(n):
        x[i] = (b[i] - sum(L[i][j] * x[j] for j in range(i))) / L[i][i]
    return x

def backward_sub(U, b):
    n = len(b)
    x = [0.0] * n
    for i in range(n - 1, -1, -1):
        # Fill in back-substitution
        pass
    return x

def solve_lu(L, U, b):
    y = forward_sub(L, b)
    return backward_sub(U, y)

L = [[1, 0, 0], [2, 1, 0], [4, 3, 1]]
U = [[2, 1, 1], [0, 1, 1], [0, 0, 2]]
b = [1, 3, 9]
x = solve_lu(L, U, b)
print(" ".join(f"{v:.4f}" for v in x))
`,

	solution: `def forward_sub(L, b):
    n = len(b)
    x = [0.0] * n
    for i in range(n):
        x[i] = (b[i] - sum(L[i][j] * x[j] for j in range(i))) / L[i][i]
    return x

def backward_sub(U, b):
    n = len(b)
    x = [0.0] * n
    for i in range(n - 1, -1, -1):
        x[i] = (b[i] - sum(U[i][j] * x[j] for j in range(i + 1, n))) / U[i][i]
    return x

def solve_lu(L, U, b):
    y = forward_sub(L, b)
    return backward_sub(U, y)

L = [[1, 0, 0], [2, 1, 0], [4, 3, 1]]
U = [[2, 1, 1], [0, 1, 1], [0, 0, 2]]
b = [1, 3, 9]
x = solve_lu(L, U, b)
print(" ".join(f"{v:.4f}" for v in x))
`,

	tests: [
		{
			name: "solve_lu for L=[[1,0,0],[2,1,0],[4,3,1]], U=[[2,1,1],[0,1,1],[0,0,2]], b=[1,3,9]",
			code: `{{FUNC}}
L = [[1, 0, 0], [2, 1, 0], [4, 3, 1]]
U = [[2, 1, 1], [0, 1, 1], [0, 0, 2]]
b = [1, 3, 9]
x = solve_lu(L, U, b)
print(" ".join(f"{v:.4f}" for v in x))`,
			expected: "0.0000 0.0000 1.0000\n",
		},
		{
			name: "forward_sub [[1,0],[2,1]] b=[2,8] → y=[2,4]",
			code: `{{FUNC}}
y = forward_sub([[1, 0], [2, 1]], [2, 8])
print(" ".join(f"{v:.4f}" for v in y))`,
			expected: "2.0000 4.0000\n",
		},
		{
			name: "backward_sub [[2,1],[0,3]] b=[8,6] → x=[3,2]",
			code: `{{FUNC}}
x = backward_sub([[2, 1], [0, 3]], [8, 6])
print(" ".join(f"{v:.4f}" for v in x))`,
			expected: "3.0000 2.0000\n",
		},
		{
			name: "solve 2×2 system [[2,1],[4,3]]x=[4,10] → x=[1,2]",
			code: `{{FUNC}}
L = [[1, 0], [2, 1]]
U = [[2, 1], [0, 1]]
b = [4, 10]
x = solve_lu(L, U, b)
print(" ".join(f"{v:.4f}" for v in x))`,
			expected: "1.0000 2.0000\n",
		},
	],
};
