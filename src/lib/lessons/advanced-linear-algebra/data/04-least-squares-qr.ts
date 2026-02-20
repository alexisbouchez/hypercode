import type { Lesson } from "../../types";

export const leastSquaresQrLesson: Lesson = {
	id: "least-squares-qr",
	title: "Least Squares via QR",
	chapterId: "orthogonality",
	content: `## Least Squares via QR

For an overdetermined system Ax ≈ b (more equations than unknowns), the least-squares solution minimises ‖Ax − b‖².

### Why QR?

The normal equations (A^T A x = A^T b) work but are numerically unstable — squaring A doubles the condition number. QR decomposition solves the same problem with far better numerical stability.

### Algorithm

Given thin QR decomposition A = QR (Q is m×n, R is n×n):

\`\`\`
Rx = Q^T b
\`\`\`

Solve this upper-triangular system via back-substitution — done.

### Example

Fit y = a + bx to data: (1, 2), (2, 4), (3, 5), (4, 4)

\`\`\`
A = [[1, 1],   b = [2,    Solution: a = 2.0000
     [1, 2],        4,               b = 0.7000
     [1, 3],        5,
     [1, 4]]        4]
\`\`\`

The best-fit line is y = 2.0 + 0.7x.

### Your Task

Implement \`least_squares_qr(A, b)\` that solves the least-squares problem using QR decomposition. Build on \`qr_decompose\` and add \`backward_sub\` for back-substitution.`,

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
            r = dot(col, q)
            R[i][j] = r
            v = [v[k] - r * q[k] for k in range(m)]
        norm = math.sqrt(dot(v, v))
        R[j][j] = norm
        Q_cols.append([x / norm for x in v])
    Q = [[Q_cols[j][i] for j in range(n)] for i in range(m)]
    return Q, R

def backward_sub(U, b):
    # Solve upper-triangular system Ux = b
    n = len(b)
    x = [0.0] * n
    for i in range(n - 1, -1, -1):
        x[i] = b[i]
        # subtract already-solved components
    return x

def least_squares_qr(A, b):
    Q, R = qr_decompose(A)
    m, n = len(Q), len(Q[0])
    # Compute Q^T b: element j = sum_i Q[i][j] * b[i]
    Qtb = [0.0] * n
    return backward_sub(R, Qtb)

A = [[1, 1], [1, 2], [1, 3], [1, 4]]
b = [2, 4, 5, 4]
x = least_squares_qr(A, b)
print(f"{x[0]:.4f}")
print(f"{x[1]:.4f}")
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

def backward_sub(U, b):
    n = len(b)
    x = [0.0] * n
    for i in range(n - 1, -1, -1):
        x[i] = (b[i] - sum(U[i][j] * x[j] for j in range(i + 1, n))) / U[i][i]
    return x

def least_squares_qr(A, b):
    Q, R = qr_decompose(A)
    m, n = len(Q), len(Q[0])
    Qtb = [sum(Q[i][j] * b[i] for i in range(m)) for j in range(n)]
    return backward_sub(R, Qtb)

A = [[1, 1], [1, 2], [1, 3], [1, 4]]
b = [2, 4, 5, 4]
x = least_squares_qr(A, b)
print(f"{x[0]:.4f}")
print(f"{x[1]:.4f}")
`,

	tests: [
		{
			name: "fit y=a+bx to (1,2),(2,4),(3,5),(4,4) — intercept a=2.0",
			code: `{{FUNC}}
A = [[1, 1], [1, 2], [1, 3], [1, 4]]
b = [2, 4, 5, 4]
x = least_squares_qr(A, b)
print(f"{x[0]:.4f}")`,
			expected: "2.0000\n",
		},
		{
			name: "fit y=a+bx to (1,2),(2,4),(3,5),(4,4) — slope b=0.7",
			code: `{{FUNC}}
A = [[1, 1], [1, 2], [1, 3], [1, 4]]
b = [2, 4, 5, 4]
x = least_squares_qr(A, b)
print(f"{x[1]:.4f}")`,
			expected: "0.7000\n",
		},
		{
			name: "exact system [[1,0],[0,1]] x = [3,5] → x=[3,5]",
			code: `{{FUNC}}
A = [[1, 0], [0, 1]]
b = [3, 5]
x = least_squares_qr(A, b)
print(f"{x[0]:.4f}")
print(f"{x[1]:.4f}")`,
			expected: "3.0000\n5.0000\n",
		},
		{
			name: "fit y=bx (no intercept) to (1,2),(2,4),(3,6) — slope=2",
			code: `{{FUNC}}
A = [[1], [2], [3]]
b = [2, 4, 6]
x = least_squares_qr(A, b)
print(f"{x[0]:.4f}")`,
			expected: "2.0000\n",
		},
	],
};
