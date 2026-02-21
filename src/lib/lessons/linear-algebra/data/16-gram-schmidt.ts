import type { Lesson } from "../../types";

export const gramSchmidt: Lesson = {
	id: "gram-schmidt",
	title: "Gram-Schmidt Orthogonalization",
	chapterId: "decompositions",
	content: `## Gram-Schmidt Orthogonalization

Given a set of linearly independent vectors, **Gram-Schmidt** produces an **orthonormal basis** spanning the same subspace.

### Algorithm

For vectors $\\mathbf{v}_1, \\mathbf{v}_2, \\ldots, \\mathbf{v}_n$:

1. $\\mathbf{e}_1 = \\mathbf{v}_1 / \\|\\mathbf{v}_1\\|$
2. For each $\\mathbf{v}_k$ ($k \\geq 2$): subtract projections onto all previous basis vectors, then normalize:

$$\\mathbf{u}_k = \\mathbf{v}_k - \\sum_{j=1}^{k-1} (\\mathbf{v}_k \\cdot \\mathbf{e}_j)\\,\\mathbf{e}_j$$
$$\\mathbf{e}_k = \\mathbf{u}_k / \\|\\mathbf{u}_k\\|$$

### Example

$\\mathbf{v}_1 = [1, 1]$, $\\mathbf{v}_2 = [0, 1]$:

$$\\mathbf{e}_1 = \\frac{1}{\\sqrt{2}}[1, 1] \\approx [0.7071, 0.7071]$$

$$\\mathbf{u}_2 = [0,1] - \\frac{1}{\\sqrt{2}} \\cdot \\frac{1}{\\sqrt{2}}[1,1] = [-0.5, 0.5]$$

$$\\mathbf{e}_2 = \\frac{1}{1/\\sqrt{2}}[-0.5, 0.5] = [-0.7071, 0.7071]$$

**Verify**: $\\mathbf{e}_1 \\cdot \\mathbf{e}_2 = -0.5 + 0.5 = 0$ ✓

### Connection to QR Decomposition

Gram-Schmidt is the algorithm behind **QR decomposition**: $A = QR$ where $Q$ has the orthonormal vectors as columns and $R$ is upper-triangular.

\`\`\`python
import math

def gram_schmidt(vecs):
    basis = []
    for v in vecs:
        u = v[:]
        for e in basis:
            proj = sum(v[i]*e[i] for i in range(len(v)))
            u = [u[i] - proj*e[i] for i in range(len(u))]
        norm = math.sqrt(sum(x**2 for x in u))
        basis.append([x/norm for x in u])
    return basis
\`\`\`

### Your Task

Implement \`gram_schmidt(vecs)\` that returns a list of orthonormal basis vectors.`,

	starterCode: `import math

def gram_schmidt(vecs):
    basis = []
    for v in vecs:
        # subtract projections onto all previous basis vectors
        # then normalize
        pass
    return basis

def dot(a, b): return sum(a[i]*b[i] for i in range(len(a)))
def norm(v): return math.sqrt(dot(v, v))

Q = gram_schmidt([[1,1],[0,1]])
print(round(dot(Q[0], Q[1]), 4))  # orthogonal → 0.0
print(round(norm(Q[0]), 4))       # unit length → 1.0
`,

	solution: `import math

def gram_schmidt(vecs):
    basis = []
    for v in vecs:
        u = v[:]
        for e in basis:
            proj = sum(v[i] * e[i] for i in range(len(v)))
            u = [u[i] - proj * e[i] for i in range(len(u))]
        norm_u = math.sqrt(sum(x**2 for x in u))
        basis.append([x / norm_u for x in u])
    return basis

def dot(a, b): return sum(a[i]*b[i] for i in range(len(a)))
def norm(v): return math.sqrt(dot(v, v))

Q = gram_schmidt([[1,1],[0,1]])
print(round(dot(Q[0], Q[1]), 4))
print(round(norm(Q[0]), 4))
`,

	tests: [
		{
			name: "e1·e2 = 0.0 (orthogonal) and ‖e1‖ = 1.0",
			expected: "0.0\n1.0\n",
		},
		{
			name: "norm of e2 = 1.0",
			code: `{{FUNC}}
import math
def norm(v): return math.sqrt(sum(x**2 for x in v))
Q = gram_schmidt([[1,1],[0,1]])
print(round(norm(Q[1]), 4))`,
			expected: "1.0\n",
		},
		{
			name: "orthonormal basis from standard basis → identity",
			code: `{{FUNC}}
import math
def dot(a, b): return sum(a[i]*b[i] for i in range(len(a)))
Q = gram_schmidt([[1,0,0],[0,1,0],[0,0,1]])
print(round(dot(Q[0], Q[1]), 4))
print(round(dot(Q[0], Q[2]), 4))`,
			expected: "0.0\n0.0\n",
		},
		{
			name: "3D basis: all pairs orthogonal",
			code: `{{FUNC}}
import math
def dot(a, b): return sum(a[i]*b[i] for i in range(len(a)))
Q = gram_schmidt([[1,1,0],[0,1,1],[1,0,1]])
print(abs(round(dot(Q[0], Q[1]), 4)))
print(abs(round(dot(Q[1], Q[2]), 4)))`,
			expected: "0.0\n0.0\n",
		},
	],
};
