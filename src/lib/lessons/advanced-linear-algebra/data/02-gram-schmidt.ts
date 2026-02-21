import type { Lesson } from "../../types";

export const gramSchmidtLesson: Lesson = {
	id: "gram-schmidt",
	title: "Gram-Schmidt Orthogonalization",
	chapterId: "orthogonality",
	content: `## Gram-Schmidt Orthogonalization

Given a set of linearly independent vectors, Gram-Schmidt produces an **orthonormal basis** — a set of vectors that are mutually perpendicular and each have unit length.

### Algorithm

For each vector **v** in turn:
1. Subtract its projection onto every previously found basis vector
2. Normalize the result

$$\\mathbf{q}_{1} = \\frac{\\mathbf{v}_{1}}{\\|\\mathbf{v}_{1}\\|}$$

For $i = 2, 3, \\ldots$:

$$\\mathbf{w}_{i} = \\mathbf{v}_{i} - \\sum_{j < i} (\\mathbf{v}_{i} \\cdot \\mathbf{q}_{j})\\, \\mathbf{q}_{j}, \\qquad \\mathbf{q}_{i} = \\frac{\\mathbf{w}_{i}}{\\|\\mathbf{w}_{i}\\|}$$

Since each $\\mathbf{q}_{j}$ is already normalized (length 1), the projection coefficient simplifies to just $\\mathbf{v}_{i} \\cdot \\mathbf{q}_{j}$.

### Example

Input vectors: [1,1,0], [1,0,1], [0,1,1]

| Step | Result |
|------|--------|
| $\\mathbf{q}_{1} = [1,1,0]/\\sqrt{2}$ | [0.7071, 0.7071, 0.0000] |
| $\\mathbf{q}_{2}$ from [1,0,1] | [0.4082, -0.4082, 0.8165] |
| $\\mathbf{q}_{3}$ from [0,1,1] | [-0.5774, 0.5774, 0.5774] |

### Your Task

Implement \`gram_schmidt(vecs)\` that takes a list of linearly independent vectors and returns an orthonormal basis.`,

	starterCode: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def gram_schmidt(vecs):
    basis = []
    for v in vecs:
        w = list(v)
        for q in basis:
            # Subtract projection of w onto q
            pass
        # Normalize w and add to basis
        pass
    return basis

Q = gram_schmidt([[1, 1, 0], [1, 0, 1], [0, 1, 1]])
for q in Q:
    print(" ".join(f"{x:.4f}" for x in q))
`,

	solution: `import math

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def gram_schmidt(vecs):
    basis = []
    for v in vecs:
        w = list(v)
        for q in basis:
            coeff = dot(w, q)
            w = [w[i] - coeff * q[i] for i in range(len(w))]
        n = math.sqrt(dot(w, w))
        basis.append([x / n for x in w])
    return basis

Q = gram_schmidt([[1, 1, 0], [1, 0, 1], [0, 1, 1]])
for q in Q:
    print(" ".join(f"{x:.4f}" for x in q))
`,

	tests: [
		{
			name: "gram_schmidt([[1,1,0],[1,0,1],[0,1,1]]) — first vector",
			code: `{{FUNC}}
Q = gram_schmidt([[1, 1, 0], [1, 0, 1], [0, 1, 1]])
print(" ".join(f"{x:.4f}" for x in Q[0]))`,
			expected: "0.7071 0.7071 0.0000\n",
		},
		{
			name: "gram_schmidt — second vector",
			code: `{{FUNC}}
Q = gram_schmidt([[1, 1, 0], [1, 0, 1], [0, 1, 1]])
print(" ".join(f"{x:.4f}" for x in Q[1]))`,
			expected: "0.4082 -0.4082 0.8165\n",
		},
		{
			name: "gram_schmidt — third vector",
			code: `{{FUNC}}
Q = gram_schmidt([[1, 1, 0], [1, 0, 1], [0, 1, 1]])
print(" ".join(f"{x:.4f}" for x in Q[2]))`,
			expected: "-0.5774 0.5774 0.5774\n",
		},
		{
			name: "gram_schmidt([[1,0],[1,1]]) — orthonormal 2D basis",
			code: `{{FUNC}}
Q = gram_schmidt([[1, 0], [1, 1]])
for q in Q:
    print(" ".join(f"{x:.4f}" for x in q))`,
			expected: "1.0000 0.0000\n0.0000 1.0000\n",
		},
	],
};
