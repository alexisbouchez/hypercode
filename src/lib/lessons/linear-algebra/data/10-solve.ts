import type { Lesson } from "../../types";

export const solve: Lesson = {
	id: "solve",
	title: "Solving Ax = b",
	chapterId: "systems",
	content: `## Solving a Linear System

A **system of linear equations** like:

\`\`\`
2x + y = 5
 x + y = 3
\`\`\`

can be written in matrix form as \`Ax = b\`:

\`\`\`python
import numpy as np

A = np.array([[2, 1],
              [1, 1]])
b = np.array([5, 3])

x = np.linalg.solve(A, b)
print(x)  # [2. 1.]  →  x=2, y=1
\`\`\`

### Why not \`A⁻¹b\`?

Mathematically, \`x = A⁻¹b\`. But **never compute the inverse just to solve a system**:

- \`np.linalg.solve\` uses LU decomposition — faster and more numerically stable
- Computing \`inv(A) @ b\` accumulates more floating point error

### Verifying the Solution

Always check: \`A @ x\` should equal \`b\` (up to floating point):

\`\`\`python
print(np.allclose(A @ x, b))  # True
\`\`\`

### When There's No Solution

If \`A\` is singular (det = 0), \`np.linalg.solve\` raises a \`LinAlgError\`. The system either has no solution or infinitely many — use \`np.linalg.lstsq\` instead.

### Your Task

Implement \`solve(A, b)\` that returns the solution vector \`x\` to \`Ax = b\`, rounded to 1 decimal place.`,

	starterCode: `import numpy as np

def solve(A, b):
    # Return the solution x to Ax = b, rounded to 1 decimal place
    pass

A = np.array([[2.0, 1.0], [1.0, 1.0]])
b = np.array([5.0, 3.0])
print(solve(A, b))
`,

	solution: `import numpy as np

def solve(A, b):
    return np.round(np.linalg.solve(A, b), 1)

A = np.array([[2.0, 1.0], [1.0, 1.0]])
b = np.array([5.0, 3.0])
print(solve(A, b))
`,

	tests: [
		{
			name: "2x+y=5, x+y=3 → x=2, y=1",
			expected: "[2. 1.]\n",
		},
		{
			name: "diagonal system",
			code: `{{FUNC}}
A = np.array([[2.0, 0.0], [0.0, 3.0]])
b = np.array([6.0, 9.0])
print(solve(A, b))`,
			expected: "[3. 3.]\n",
		},
		{
			name: "identity system: x = b",
			code: `{{FUNC}}
A = np.eye(3)
b = np.array([1.0, 2.0, 3.0])
print(solve(A, b))`,
			expected: "[1. 2. 3.]\n",
		},
		{
			name: "solution satisfies Ax = b",
			code: `{{FUNC}}
A = np.array([[3.0, 1.0], [1.0, 2.0]])
b = np.array([9.0, 8.0])
x = solve(A, b)
print(np.allclose(A @ x, b))`,
			expected: "True\n",
		},
	],
};
