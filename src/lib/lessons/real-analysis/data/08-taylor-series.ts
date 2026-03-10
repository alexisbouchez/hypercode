import type { Lesson } from "../../types";

export const taylorSeries: Lesson = {
	id: "taylor-series",
	title: "Taylor Series",
	chapterId: "series",
	content: `## Taylor Series

The **Taylor series** of a function \\(f\\) centered at \\(a\\) is:

$$f(x) = \\sum_{n=0}^{\\infty} \\frac{f^{(n)}(a)}{n!} (x - a)^n$$

When \\(a = 0\\), this is called a **Maclaurin series**.

### Key Taylor Series

| Function | Maclaurin series |
|----------|-----------------|
| \\(e^x\\) | \\(\\sum_{n=0}^{\\infty} \\frac{x^n}{n!}\\) |
| \\(\\sin(x)\\) | \\(\\sum_{n=0}^{\\infty} \\frac{(-1)^n x^{2n+1}}{(2n+1)!}\\) |
| \\(\\cos(x)\\) | \\(\\sum_{n=0}^{\\infty} \\frac{(-1)^n x^{2n}}{(2n)!}\\) |

### Approximation Error

The Taylor polynomial of degree \\(N\\) provides an approximation. The error decreases as \\(N\\) increases (within the radius of convergence).

### Numerical Taylor Approximation

We can compute Taylor polynomials term by term:

\`\`\`python
import math

def taylor_exp(x, N):
    return sum(x**n / math.factorial(n) for n in range(N + 1))
\`\`\`

### Your Task

Implement:
1. \`taylor_sin(x, N)\` -- compute the degree-\\(N\\) Taylor approximation of \\(\\sin(x)\\) using terms up to index \\(N\\): \\(\\sum_{n=0}^{N} \\frac{(-1)^n x^{2n+1}}{(2n+1)!}\\)
2. \`taylor_cos(x, N)\` -- compute \\(\\sum_{n=0}^{N} \\frac{(-1)^n x^{2n}}{(2n)!}\\)
3. \`taylor_exp(x, N)\` -- compute \\(\\sum_{n=0}^{N} \\frac{x^n}{n!}\\)`,

	starterCode: `import math

def taylor_sin(x, N):
    # sum of (-1)^n * x^(2n+1) / (2n+1)! for n = 0 to N
    pass

def taylor_cos(x, N):
    # sum of (-1)^n * x^(2n) / (2n)! for n = 0 to N
    pass

def taylor_exp(x, N):
    # sum of x^n / n! for n = 0 to N
    pass

print(round(taylor_sin(math.pi / 2, 10), 4))
print(round(taylor_cos(0, 10), 4))
print(round(taylor_exp(1, 15), 4))
`,

	solution: `import math

def taylor_sin(x, N):
    return sum((-1)**n * x**(2*n+1) / math.factorial(2*n+1) for n in range(N + 1))

def taylor_cos(x, N):
    return sum((-1)**n * x**(2*n) / math.factorial(2*n) for n in range(N + 1))

def taylor_exp(x, N):
    return sum(x**n / math.factorial(n) for n in range(N + 1))

print(round(taylor_sin(math.pi / 2, 10), 4))
print(round(taylor_cos(0, 10), 4))
print(round(taylor_exp(1, 15), 4))
`,

	tests: [
		{
			name: "sin(pi/2) approximation equals 1",
			code: `{{FUNC}}
import math
print(round(taylor_sin(math.pi / 2, 10), 4))`,
			expected: "1.0\n",
		},
		{
			name: "cos(0) equals 1",
			code: `{{FUNC}}
import math
print(round(taylor_cos(0, 10), 4))`,
			expected: "1.0\n",
		},
		{
			name: "exp(1) approximation equals e",
			code: `{{FUNC}}
import math
print(round(taylor_exp(1, 15), 4))`,
			expected: "2.7183\n",
		},
		{
			name: "sin(pi) is approximately 0",
			code: `{{FUNC}}
import math
print(round(taylor_sin(math.pi, 12), 4))`,
			expected: "0.0\n",
		},
	],
};
