import type { Lesson } from "../../types";

export const solve: Lesson = {
	id: "solve",
	title: "Solving Ax = b",
	chapterId: "systems",
	content: `## Solving a Linear System

A **system of linear equations** like:

$$2x + y = 5$$
$$x + y = 3$$

can be written in matrix form as $\mathbf{A}\mathbf{x} = \mathbf{b}$:

\`\`\`python
A = [[2, 1],
     [1, 1]]
b = [5, 3]
\`\`\`

### Gaussian Elimination

We solve this by **row reduction** — transforming $\mathbf{A}$ into upper triangular form, then back-substituting:

\`\`\`python
def solve(A, b):
    n = len(b)
    # Augment [A|b]
    M = [A[i][:] + [b[i]] for i in range(n)]
    # Forward elimination
    for col in range(n):
        for row in range(col+1, n):
            factor = M[row][col] / M[col][col]
            M[row] = [M[row][k] - factor*M[col][k] for k in range(n+1)]
    # Back substitution
    x = [0.0] * n
    for i in range(n-1, -1, -1):
        x[i] = (M[i][n] - sum(M[i][j]*x[j] for j in range(i+1, n))) / M[i][i]
    return [round(xi, 1) for xi in x]

x = solve([[2,1],[1,1]], [5,3])
print(x)  # [2.0, 1.0]  →  x=2, y=1
\`\`\`

### Why Gaussian Elimination?

- More numerically stable than computing the inverse
- Works directly on the augmented matrix $[\mathbf{A} \mid \mathbf{b}]$
- Basis for LU decomposition

### Verifying the Solution

Check: $\mathbf{A}\mathbf{x}$ should equal $\mathbf{b}$:

\`\`\`python
Ax = [sum(A[i][j]*x[j] for j in range(len(x))) for i in range(len(A))]
print(all(abs(Ax[i] - b[i]) < 1e-9 for i in range(len(b))))  # True
\`\`\`

### Your Task

Implement \`solve(A, b)\` that returns the solution vector $\mathbf{x}$ to $\mathbf{A}\mathbf{x} = \mathbf{b}$, rounded to 1 decimal place.`,

	starterCode: `def solve(A, b):
    # Return the solution x to Ax = b, rounded to 1 decimal place
    # Use Gaussian elimination
    pass

A = [[2.0, 1.0], [1.0, 1.0]]
b = [5.0, 3.0]
print(solve(A, b))
`,

	solution: `def solve(A, b):
    n = len(b)
    M = [A[i][:] + [b[i]] for i in range(n)]
    for col in range(n):
        for row in range(col + 1, n):
            factor = M[row][col] / M[col][col]
            M[row] = [M[row][k] - factor * M[col][k] for k in range(n + 1)]
    x = [0.0] * n
    for i in range(n - 1, -1, -1):
        x[i] = (M[i][n] - sum(M[i][j] * x[j] for j in range(i + 1, n))) / M[i][i]
    return [round(xi, 1) for xi in x]

A = [[2.0, 1.0], [1.0, 1.0]]
b = [5.0, 3.0]
print(solve(A, b))
`,

	tests: [
		{
			name: "2x+y=5, x+y=3 → x=2, y=1",
			expected: "[2.0, 1.0]\n",
		},
		{
			name: "diagonal system",
			code: `{{FUNC}}
A = [[2.0, 0.0], [0.0, 3.0]]
b = [6.0, 9.0]
print(solve(A, b))`,
			expected: "[3.0, 3.0]\n",
		},
		{
			name: "identity system: x = b",
			code: `{{FUNC}}
A = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
b = [1.0, 2.0, 3.0]
print(solve(A, b))`,
			expected: "[1.0, 2.0, 3.0]\n",
		},
		{
			name: "solution satisfies Ax = b",
			code: `{{FUNC}}
A = [[3.0, 1.0], [1.0, 2.0]]
b = [9.0, 8.0]
x = solve(A, b)
Ax = [sum(A[i][j]*x[j] for j in range(len(x))) for i in range(len(A))]
print(all(abs(Ax[i] - b[i]) < 1e-6 for i in range(len(b))))`,
			expected: "True\n",
		},
	],
};
