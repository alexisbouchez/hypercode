import type { Lesson } from "../../types";

export const inverse: Lesson = {
	id: "inverse",
	title: "Matrix Inverse",
	chapterId: "systems",
	content: `## The Matrix Inverse

The **inverse** of a matrix \`A\` is the matrix \`A⁻¹\` such that:

\`\`\`
A · A⁻¹ = A⁻¹ · A = I
\`\`\`

\`\`\`python
import numpy as np

A = np.array([[2.0, 1.0],
              [1.0, 1.0]])

Ainv = np.linalg.inv(A)
print(Ainv)
# [[ 1. -1.]
#  [-1.  2.]]

# Verify: A @ A⁻¹ ≈ I
print(np.round(A @ Ainv).astype(int))
# [[1 0]
#  [0 1]]
\`\`\`

### When Does an Inverse Exist?

A matrix is **invertible** (non-singular) if and only if its determinant is non-zero:

\`\`\`python
# Singular matrix — no inverse
S = np.array([[1, 2], [2, 4]])   # det = 0
np.linalg.inv(S)  # raises LinAlgError
\`\`\`

### Why Inverses Matter

The equation \`Ax = b\` can be solved as \`x = A⁻¹b\` — *if* \`A\` is invertible. In practice, use \`np.linalg.solve\` instead (it's faster and more numerically stable than computing the inverse explicitly).

### Your Task

Implement \`is_invertible(A)\` that returns \`True\` if the matrix is invertible, \`False\` otherwise. Use the determinant.`,

	starterCode: `import numpy as np

def is_invertible(A):
    # Return True if A is invertible (det != 0), False otherwise
    pass

A = np.array([[2, 1], [1, 1]])
S = np.array([[1, 2], [2, 4]])
print(is_invertible(A))
print(is_invertible(S))
`,

	solution: `import numpy as np

def is_invertible(A):
    return abs(np.linalg.det(A)) > 1e-10

A = np.array([[2, 1], [1, 1]])
S = np.array([[1, 2], [2, 4]])
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
print(is_invertible(np.eye(3)))`,
			expected: "True\n",
		},
		{
			name: "zero matrix is not invertible",
			code: `{{FUNC}}
print(is_invertible(np.zeros((3, 3))))`,
			expected: "False\n",
		},
		{
			name: "diagonal matrix is invertible iff no zero on diagonal",
			code: `{{FUNC}}
D1 = np.array([[2, 0], [0, 3]])
D2 = np.array([[2, 0], [0, 0]])
print(is_invertible(D1))
print(is_invertible(D2))`,
			expected: "True\nFalse\n",
		},
	],
};
