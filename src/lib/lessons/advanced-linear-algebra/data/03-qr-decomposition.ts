import type { Lesson } from "../../types";

export const qrDecompositionLesson: Lesson = {
	id: "qr-decomposition",
	title: "QR Decomposition",
	chapterId: "orthogonality",
	content: `## QR Decomposition

Every matrix A can be factored as **A = QR** where:
- **Q** has orthonormal columns (Q^T Q = I)
- **R** is upper triangular with positive diagonal entries

QR decomposition is the foundation for numerically stable least-squares, eigenvalue algorithms, and more.

### Algorithm via Gram-Schmidt

Process each column of A left-to-right:
- Apply Gram-Schmidt to produce orthonormal column qⱼ
- R[i][j] = dot(aⱼ, qᵢ) for i < j  (how much of each previous q is in aⱼ)
- R[j][j] = ‖wⱼ‖  (the norm of the residual)

### Example

\`\`\`
A = [[3, 2],    Q = [[0.9487, -0.3162],    R = [[3.1623, 2.5298],
     [1, 2]]         [0.3162,  0.9487]]         [0.0000, 1.2649]]
\`\`\`

Verify: Q^T Q = I and Q × R = A.

### Your Task

Implement \`qr_decompose(A)\` returning \`(Q, R)\`. Use Gram-Schmidt on the columns of A.`,

	starterCode: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def qr_decompose(A):
    m, n = len(A), len(A[0])
    cols = [[A[i][j] for i in range(m)] for j in range(n)]
    Q_cols = []
    R = [[0.0] * n for _ in range(n)]
    for j, col in enumerate(cols):
        v = list(col)
        for i, q in enumerate(Q_cols):
            # R[i][j] = dot(col, q); subtract projection from v
            pass
        # R[j][j] = norm of v; normalize v and add to Q_cols
        pass
    Q = [[Q_cols[j][i] for j in range(n)] for i in range(m)]
    return Q, R

A = [[3, 2], [1, 2]]
Q, R = qr_decompose(A)
for row in Q:
    print(" ".join(f"{x:.4f}" for x in row))
print("---")
for row in R:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	solution: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def qr_decompose(A):
    m, n = len(A), len(A[0])
    cols = [[A[i][j] for i in range(m)] for j in range(n)]
    Q_cols = []
    R = [[0.0] * n for _ in range(n)]
    for j, col in enumerate(cols):
        v = list(col)
        for i, q in enumerate(Q_cols):
            r = dot(col, q)
            R[i][j] = r
            v = [v[k] - r * q[k] for k in range(m)]
        norm = math.sqrt(dot(v, v))
        R[j][j] = norm
        Q_cols.append([x / norm for x in v])
    Q = [[Q_cols[j][i] for j in range(n)] for i in range(m)]
    return Q, R

A = [[3, 2], [1, 2]]
Q, R = qr_decompose(A)
for row in Q:
    print(" ".join(f"{x:.4f}" for x in row))
print("---")
for row in R:
    print(" ".join(f"{x:.4f}" for x in row))
`,

	tests: [
		{
			name: "Q matrix of [[3,2],[1,2]]",
			code: `{{FUNC}}
Q, R = qr_decompose([[3, 2], [1, 2]])
for row in Q:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "0.9487 -0.3162\n0.3162 0.9487\n",
		},
		{
			name: "R matrix of [[3,2],[1,2]]",
			code: `{{FUNC}}
Q, R = qr_decompose([[3, 2], [1, 2]])
for row in R:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "3.1623 2.5298\n0.0000 1.2649\n",
		},
		{
			name: "Q^T Q = I for [[3,2],[1,2]]",
			code: `{{FUNC}}
Q, R = qr_decompose([[3, 2], [1, 2]])
n = len(Q[0])
QtQ = [[sum(Q[k][i]*Q[k][j] for k in range(len(Q))) for j in range(n)] for i in range(n)]
for row in QtQ:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "1.0000 0.0000\n0.0000 1.0000\n",
		},
		{
			name: "QR = A for [[3,2],[1,2]]",
			code: `{{FUNC}}
A = [[3, 2], [1, 2]]
Q, R = qr_decompose(A)
n = len(R)
QR = [[sum(Q[i][k]*R[k][j] for k in range(n)) for j in range(n)] for i in range(len(Q))]
for row in QR:
    print(" ".join(f"{x:.4f}" for x in row))`,
			expected: "3.0000 2.0000\n1.0000 2.0000\n",
		},
	],
};
