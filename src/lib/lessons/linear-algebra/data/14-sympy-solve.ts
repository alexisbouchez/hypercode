import type { Lesson } from "../../types";

export const sympySolve: Lesson = {
	id: "sympy-solve",
	title: "Solving Equations Symbolically",
	chapterId: "symbolic",
	content: `## Solving Equations with SymPy

\`sympy.solve(expr, x)\` finds all values of \`x\` that make \`expr = 0\`.

\`\`\`python
import sympy

x = sympy.Symbol('x')

# Solve x - 3 = 0
print(sympy.solve(x - 3, x))      # [3]

# Solve x² - 5x + 6 = 0
print(sympy.solve(x**2 - 5*x + 6, x))  # [2, 3]

# Solve x² + 1 = 0 (no real solutions)
print(sympy.solve(x**2 + 1, x))   # [-I, I]  (complex)
\`\`\`

### Systems of Equations

Solve multiple equations simultaneously:

\`\`\`python
x, y = sympy.symbols('x y')

solution = sympy.solve([
    x + y - 5,    # x + y = 5
    x - y - 1,    # x - y = 1
], [x, y])

print(solution)  # {x: 3, y: 2}
\`\`\`

### Symbolic vs Numeric

| Feature | SymPy | NumPy |
|---------|-------|-------|
| Result | Exact (e.g. \`sqrt(2)\`) | Approximate (e.g. \`1.41421...\`) |
| Variables | Symbolic | Arrays of numbers |
| Speed | Slow (general) | Fast (vectorized) |

Use SymPy when you need **exact answers** or **general formulas**. Use NumPy when you have specific numerical data.

### Your Task

Implement \`solve_linear(a, b)\` that solves \`a·x + b = 0\` and returns the solution as a string.`,

	starterCode: `import sympy

def solve_linear(a, b):
    # Solve a*x + b = 0, return str of the single solution
    x = sympy.Symbol('x')
    pass

print(solve_linear(2, -6))
print(solve_linear(3, 9))
print(solve_linear(1, -5))
`,

	solution: `import sympy

def solve_linear(a, b):
    x = sympy.Symbol('x')
    solutions = sympy.solve(a * x + b, x)
    return str(solutions[0])

print(solve_linear(2, -6))
print(solve_linear(3, 9))
print(solve_linear(1, -5))
`,

	tests: [
		{
			name: "2x-6=0 → 3, 3x+9=0 → -3, x-5=0 → 5",
			expected: "3\n-3\n5\n",
		},
		{
			name: "solve 4x + 8 = 0 → -2",
			code: `{{FUNC}}
print(solve_linear(4, 8))`,
			expected: "-2\n",
		},
		{
			name: "solve x + 0 = 0 → 0",
			code: `{{FUNC}}
print(solve_linear(1, 0))`,
			expected: "0\n",
		},
		{
			name: "solve -x + 7 = 0 → 7",
			code: `{{FUNC}}
print(solve_linear(-1, 7))`,
			expected: "7\n",
		},
	],
};
