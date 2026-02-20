import type { Lesson } from "../../types";

export const sympyMatrices: Lesson = {
	id: "sympy-matrices",
	title: "Symbolic Matrices & Equation Rendering",
	chapterId: "symbolic",
	content: `## SymPy Matrices and Beautiful Rendering

SymPy has its own \`Matrix\` class for **exact symbolic matrix algebra**. Combined with \`pprint\`, it renders equations beautifully in your terminal.

\`\`\`python
import sympy

A = sympy.Matrix([[1, 2], [3, 4]])
sympy.pprint(A)
\`\`\`

Renders as:

\`\`\`
⎡1  2⎤
⎢    ⎥
⎣3  4⎦
\`\`\`

### Symbolic Variables in Matrices

\`\`\`python
x, y = sympy.symbols('x y')

M = sympy.Matrix([[x, x + y],
                  [x - y, y]])
sympy.pprint(M)
\`\`\`

\`\`\`
⎡  x    x + y⎤
⎢            ⎥
⎣x - y    y  ⎦
\`\`\`

### Exact Determinant

\`\`\`python
print(A.det())        # -2  (exact integer!)

x = sympy.Symbol('x')
B = sympy.Matrix([[x, 1], [1, x]])
print(B.det())        # x**2 - 1
sympy.pprint(sympy.factor(B.det()))  # (x - 1)·(x + 1)
\`\`\`

### Exact Inverse

\`\`\`python
print(A.inv())
# Matrix([[-2, 1], [3/2, -1/2]])
\`\`\`

### Eigenvalues Symbolically

\`\`\`python
print(A.eigenvals())  # {-sqrt(33)/2 + 5/2: 1, sqrt(33)/2 + 5/2: 1}
\`\`\`

### Your Task

Implement \`symbolic_det(a, b, c, d)\` that computes the determinant of the 2×2 symbolic matrix \`[[a, b], [c, d]]\` and returns it as a string.`,

	starterCode: `import sympy

def symbolic_det(a, b, c, d):
    # Create a sympy Matrix [[a, b], [c, d]] and return str(its determinant)
    pass

print(symbolic_det(1, 2, 3, 4))
print(symbolic_det(2, 0, 0, 3))

# Bonus: show pprint of a beautiful matrix
x, y = sympy.symbols('x y')
M = sympy.Matrix([[x**2, x + y], [x - y, y**2]])
sympy.pprint(M)
`,

	solution: `import sympy

def symbolic_det(a, b, c, d):
    M = sympy.Matrix([[a, b], [c, d]])
    return str(M.det())

print(symbolic_det(1, 2, 3, 4))
print(symbolic_det(2, 0, 0, 3))
`,

	tests: [
		{
			name: "det([[1,2],[3,4]]) = -2, det([[2,0],[0,3]]) = 6",
			expected: "-2\n6\n",
		},
		{
			name: "det([[1,1],[1,1]]) = 0 (singular)",
			code: `{{FUNC}}
print(symbolic_det(1, 1, 1, 1))`,
			expected: "0\n",
		},
		{
			name: "det([[5,0],[0,4]]) = 20",
			code: `{{FUNC}}
print(symbolic_det(5, 0, 0, 4))`,
			expected: "20\n",
		},
		{
			name: "det([[1,0],[0,1]]) = 1",
			code: `{{FUNC}}
print(symbolic_det(1, 0, 0, 1))`,
			expected: "1\n",
		},
	],
};
