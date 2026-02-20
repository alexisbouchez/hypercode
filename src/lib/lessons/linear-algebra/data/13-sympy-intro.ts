import type { Lesson } from "../../types";

export const sympyIntro: Lesson = {
	id: "sympy-intro",
	title: "Symbolic Math with SymPy",
	chapterId: "symbolic",
	content: `## SymPy: Computer Algebra in Python

**SymPy** is a Python library for symbolic mathematics. Unlike NumPy (which works with numbers), SymPy works with exact mathematical expressions.

\`\`\`python
import sympy

x = sympy.Symbol('x')

expr = (x + 1)**2
print(sympy.expand(expr))   # x**2 + 2*x + 1
print(sympy.factor(expr))   # (x + 1)**2

# Beautiful rendering
sympy.pprint(expr)
\`\`\`

Which prints:

\`\`\`
       2
(x + 1)
\`\`\`

### Symbols

\`sympy.Symbol('x')\` creates a symbolic variable. You can declare multiple at once:

\`\`\`python
x, y, z = sympy.symbols('x y z')
\`\`\`

### Exact Arithmetic

SymPy keeps fractions exact:

\`\`\`python
print(sympy.Rational(1, 3) + sympy.Rational(1, 6))  # 1/2
print(sympy.sqrt(8))                                 # 2*sqrt(2)
\`\`\`

### expand and factor

- \`sympy.expand(expr)\` — distribute and simplify
- \`sympy.factor(expr)\` — factor into irreducible parts
- \`sympy.simplify(expr)\` — apply various simplifications

### Your Task

Implement \`factor_quadratic(a, b, c)\` that returns a string representation of the factored form of \`ax² + bx + c\`.`,

	starterCode: `import sympy

def factor_quadratic(a, b, c):
    # Return str(sympy.factor(a*x**2 + b*x + c))
    x = sympy.Symbol('x')
    pass

print(factor_quadratic(1, 2, 1))
print(factor_quadratic(1, -1, -2))
print(factor_quadratic(1, 0, -1))
`,

	solution: `import sympy

def factor_quadratic(a, b, c):
    x = sympy.Symbol('x')
    expr = a * x**2 + b * x + c
    return str(sympy.factor(expr))

print(factor_quadratic(1, 2, 1))
print(factor_quadratic(1, -1, -2))
print(factor_quadratic(1, 0, -1))
`,

	tests: [
		{
			name: "x²+2x+1=(x+1)², x²-x-2=(x-2)(x+1), x²-1=(x-1)(x+1)",
			expected: "(x + 1)**2\n(x - 2)*(x + 1)\n(x - 1)*(x + 1)\n",
		},
		{
			name: "2x²+4x = 2x(x+2)",
			code: `{{FUNC}}
print(factor_quadratic(2, 4, 0))`,
			expected: "2*x*(x + 2)\n",
		},
		{
			name: "x²+1 is irreducible (stays as-is)",
			code: `{{FUNC}}
print(factor_quadratic(1, 0, 1))`,
			expected: "x**2 + 1\n",
		},
		{
			name: "x²-4 = (x-2)(x+2)",
			code: `{{FUNC}}
print(factor_quadratic(1, 0, -4))`,
			expected: "(x - 2)*(x + 2)\n",
		},
	],
};
