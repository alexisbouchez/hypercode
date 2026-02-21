import type { Lesson } from "../../types";

export const matrixNormsLesson: Lesson = {
	id: "matrix-norms",
	title: "Matrix Norms",
	chapterId: "matrix-analysis",
	content: `## Matrix Norms

A matrix norm $\\|A\\|$ measures the "size" of a matrix. Different norms capture different aspects of the matrix.

### Frobenius Norm

The most common: sum of squares of all entries, then square root.

$$\\|A\\|_F = \\sqrt{\\sum_{i,j} a_{ij}^2}$$

Analogous to the Euclidean length of the vector of all entries.

### Infinity Norm (Max Row Sum)

The maximum absolute row sum — the worst-case amplification when multiplying a vector with $\\|\\mathbf{v}\\|_\\infty \\leq 1$.

$$\\|A\\|_\\infty = \\max_i \\sum_j |a_{ij}|$$

### 1-Norm (Max Column Sum)

The maximum absolute column sum.

$$\\|A\\|_1 = \\max_j \\sum_i |a_{ij}|$$

### Example

$$A = \\begin{pmatrix} 1 & 2 \\\\ 3 & 4 \\end{pmatrix}$$

$$\\|A\\|_F = \\sqrt{1+4+9+16} = \\sqrt{30} \\approx 5.4772$$

$$\\|A\\|_\\infty = \\max(1+2,\\ 3+4) = 7.0000, \\qquad \\|A\\|_1 = \\max(1+3,\\ 2+4) = 6.0000$$

### Your Task

Implement \`frobenius_norm(A)\`, \`infinity_norm(A)\`, and \`one_norm(A)\`.`,

	starterCode: `import math

def frobenius_norm(A):
    return math.sqrt(sum(A[i][j]**2
                        for i in range(len(A))
                        for j in range(len(A[0]))))

def infinity_norm(A):
    # max absolute row sum
    return 0.0

def one_norm(A):
    # max absolute column sum
    return 0.0

A = [[1, 2], [3, 4]]
print(f"{frobenius_norm(A):.4f}")
print(f"{infinity_norm(A):.4f}")
print(f"{one_norm(A):.4f}")
`,

	solution: `import math

def frobenius_norm(A):
    return math.sqrt(sum(A[i][j]**2
                        for i in range(len(A))
                        for j in range(len(A[0]))))

def infinity_norm(A):
    return max(sum(abs(A[i][j]) for j in range(len(A[0])))
               for i in range(len(A)))

def one_norm(A):
    return max(sum(abs(A[i][j]) for i in range(len(A)))
               for j in range(len(A[0])))

A = [[1, 2], [3, 4]]
print(f"{frobenius_norm(A):.4f}")
print(f"{infinity_norm(A):.4f}")
print(f"{one_norm(A):.4f}")
`,

	tests: [
		{
			name: "frobenius_norm([[1,2],[3,4]]) = sqrt(30) ≈ 5.4772",
			code: `{{FUNC}}
print(f"{frobenius_norm([[1, 2], [3, 4]]):.4f}")`,
			expected: "5.4772\n",
		},
		{
			name: "infinity_norm([[1,2],[3,4]]) = 7",
			code: `{{FUNC}}
print(f"{infinity_norm([[1, 2], [3, 4]]):.4f}")`,
			expected: "7.0000\n",
		},
		{
			name: "one_norm([[1,2],[3,4]]) = 6",
			code: `{{FUNC}}
print(f"{one_norm([[1, 2], [3, 4]]):.4f}")`,
			expected: "6.0000\n",
		},
		{
			name: "frobenius_norm of identity 3×3 = sqrt(3)",
			code: `{{FUNC}}
I = [[1,0,0],[0,1,0],[0,0,1]]
print(f"{frobenius_norm(I):.4f}")`,
			expected: "1.7321\n",
		},
	],
};
