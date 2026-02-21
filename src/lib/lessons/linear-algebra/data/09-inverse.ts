import type { Lesson } from "../../types";

export const inverse: Lesson = {
	id: "inverse",
	title: "Matrix Inverse",
	chapterId: "systems",
	content: `## The Matrix Inverse

The **inverse** of a matrix $\mathbf{A}$ is the matrix $\mathbf{A}^{-1}$ such that:

$$\mathbf{A} \cdot \mathbf{A}^{-1} = \mathbf{A}^{-1} \cdot \mathbf{A} = I$$

### When Does an Inverse Exist?

A matrix is **invertible** (non-singular) if and only if its determinant is non-zero:

\`\`\`python
def det(A):
    if len(A) == 1: return A[0][0]
    if len(A) == 2: return A[0][0]*A[1][1] - A[0][1]*A[1][0]
    return sum(((-1)**j)*A[0][j]*det([[A[i][k] for k in range(len(A)) if k!=j]
                                      for i in range(1,len(A))]) for j in range(len(A)))

A = [[2, 1], [1, 1]]
S = [[1, 2], [2, 4]]  # det = 0 (singular)

print(abs(det(A)) > 1e-10)   # True
print(abs(det(S)) > 1e-10)   # False
\`\`\`

### Why Invertibility Matters

The equation $\mathbf{A}\mathbf{x} = \mathbf{b}$ can be solved as $\mathbf{x} = \mathbf{A}^{-1}\mathbf{b}$ — *if* $\mathbf{A}$ is invertible. In practice, use Gaussian elimination instead (it's faster and more numerically stable).

### Your Task

Implement \`is_invertible(A)\` that returns \`True\` if the matrix is invertible, \`False\` otherwise. Use the determinant.`,

	starterCode: `def det(A):
    if len(A) == 1: return A[0][0]
    if len(A) == 2: return A[0][0]*A[1][1] - A[0][1]*A[1][0]
    return sum(((-1)**j)*A[0][j]*det([[A[i][k] for k in range(len(A)) if k!=j]
                                      for i in range(1,len(A))]) for j in range(len(A)))

def is_invertible(A):
    # Return True if A is invertible (det != 0), False otherwise
    pass

A = [[2, 1], [1, 1]]
S = [[1, 2], [2, 4]]
print(is_invertible(A))
print(is_invertible(S))
`,

	solution: `def det(A):
    if len(A) == 1: return A[0][0]
    if len(A) == 2: return A[0][0]*A[1][1] - A[0][1]*A[1][0]
    return sum(((-1)**j)*A[0][j]*det([[A[i][k] for k in range(len(A)) if k!=j]
                                      for i in range(1,len(A))]) for j in range(len(A)))

def is_invertible(A):
    return abs(det(A)) > 1e-10

A = [[2, 1], [1, 1]]
S = [[1, 2], [2, 4]]
print(is_invertible(A))
print(is_invertible(S))
`,

	tests: [
		{
			name: "invertible and singular",
			expected: "True\nFalse\n",
		},
		{
			name: "identity is invertible",
			code: `{{FUNC}}
I = [[1 if i==j else 0 for j in range(3)] for i in range(3)]
print(is_invertible(I))`,
			expected: "True\n",
		},
		{
			name: "zero matrix is not invertible",
			code: `{{FUNC}}
Z = [[0]*3 for _ in range(3)]
print(is_invertible(Z))`,
			expected: "False\n",
		},
		{
			name: "diagonal matrix is invertible iff no zero on diagonal",
			code: `{{FUNC}}
D1 = [[2, 0], [0, 3]]
D2 = [[2, 0], [0, 0]]
print(is_invertible(D1))
print(is_invertible(D2))`,
			expected: "True\nFalse\n",
		},
	],
};
