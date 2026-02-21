import type { Lesson } from "../../types";

export const newtonRaphson: Lesson = {
	id: "newton-raphson",
	title: "Newton-Raphson Method",
	chapterId: "numerical",
	content: `## Newton's Method for Root-Finding

The **Newton-Raphson method** is an iterative algorithm for finding roots of equations — values of $x$ where $f(x) = 0$.

Starting from an initial guess $x_0$, it repeatedly improves the estimate:

$$x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}$$

\`\`\`python
def newton(f, df, x0, tol=1e-6):
    x = x0
    for _ in range(100):
        fx = f(x)
        if abs(fx) < tol:
            break
        x = x - fx / df(x)
    return round(x, 6)

# Find √2: solve x² - 2 = 0
f  = lambda x: x**2 - 2
df = lambda x: 2*x
print(newton(f, df, 1.0))   # 1.414214
\`\`\`

### Why It Works

Newton's method uses the **tangent line** at the current point to approximate where the function crosses zero. Each iteration gives quadratic convergence — the number of correct digits roughly doubles each step.

### Convergence

The method converges quickly when:
- The initial guess $x_0$ is close to the root
- $f'(x)$ is not near zero at the root

### Applications

- Root-finding in optimization (gradient = 0)
- Solving implicit equations in physics
- Computing square roots and logarithms in hardware

### Your Task

Implement \`newton(f, df, x0, tol=1e-6)\` that returns the root of $f$ starting from $x_0$, rounded to 6 decimal places.`,

	starterCode: `def newton(f, df, x0, tol=1e-6):
    # Iterate: x = x - f(x)/df(x) until |f(x)| < tol
    # Return the root rounded to 6 decimal places
    pass

# Find √2
f  = lambda x: x**2 - 2
df = lambda x: 2*x
print(newton(f, df, 1.0))
`,

	solution: `def newton(f, df, x0, tol=1e-6):
    x = x0
    for _ in range(100):
        fx = f(x)
        if abs(fx) < tol:
            break
        x = x - fx / df(x)
    return round(x, 6)

# Find √2
f  = lambda x: x**2 - 2
df = lambda x: 2*x
print(newton(f, df, 1.0))
`,

	tests: [
		{
			name: "find √2 ≈ 1.414214",
			expected: "1.414214\n",
		},
		{
			name: "find cube root of 8 = 2.0",
			code: `{{FUNC}}
f  = lambda x: x**3 - 8
df = lambda x: 3*x**2
print(newton(f, df, 1.5))`,
			expected: "2.0\n",
		},
		{
			name: "find root of x³ - x - 2 ≈ 1.521380",
			code: `{{FUNC}}
f  = lambda x: x**3 - x - 2
df = lambda x: 3*x**2 - 1
print(newton(f, df, 1.5))`,
			expected: "1.52138\n",
		},
		{
			name: "find root of x² - 9 = ±3 starting from 4.0",
			code: `{{FUNC}}
f  = lambda x: x**2 - 9
df = lambda x: 2*x
print(newton(f, df, 4.0))`,
			expected: "3.0\n",
		},
	],
};
