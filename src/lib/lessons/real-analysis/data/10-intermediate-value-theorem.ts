import type { Lesson } from "../../types";

export const intermediateValueTheorem: Lesson = {
	id: "intermediate-value-theorem",
	title: "Intermediate Value Theorem",
	chapterId: "continuity",
	content: `## Intermediate Value Theorem (IVT)

The IVT is one of the most powerful results about continuous functions.

### Statement

If \\(f\\) is continuous on \\([a, b]\\) and \\(y\\) is any value between \\(f(a)\\) and \\(f(b)\\), then there exists some \\(c \\in (a, b)\\) such that \\(f(c) = y\\).

### Consequence: Root Finding

If \\(f(a)\\) and \\(f(b)\\) have opposite signs, then \\(f\\) has a root in \\((a, b)\\). This is the basis of the **bisection method**.

### The Bisection Method

1. Start with \\([a, b]\\) where \\(f(a)\\) and \\(f(b)\\) have opposite signs
2. Compute the midpoint \\(m = (a + b) / 2\\)
3. If \\(f(a) \\cdot f(m) < 0\\), the root is in \\([a, m]\\); otherwise in \\([m, b]\\)
4. Repeat until the interval is small enough

Each step halves the interval, so after \\(n\\) steps the error is at most \\((b - a) / 2^n\\).

### Example: Finding \\(\\sqrt{2}\\)

\\(\\sqrt{2}\\) is a root of \\(f(x) = x^2 - 2\\). Since \\(f(1) = -1 < 0\\) and \\(f(2) = 2 > 0\\), the IVT guarantees a root in \\((1, 2)\\).

### Your Task

Implement:
1. \`has_root(f, a, b)\` -- returns \`True\` if \\(f(a)\\) and \\(f(b)\\) have opposite signs
2. \`bisection(f, a, b, tol)\` -- finds a root of \\(f\\) in \\([a, b]\\) using bisection, stopping when \\(b - a < \\text{tol}\\). Returns the midpoint.`,

	starterCode: `def has_root(f, a, b):
    # Return True if f(a) and f(b) have opposite signs
    pass

def bisection(f, a, b, tol):
    # Find root using bisection method
    # Stop when b - a < tol, return the midpoint
    pass

f = lambda x: x ** 2 - 2  # root is sqrt(2)
print(has_root(f, 1, 2))
print(round(bisection(f, 1, 2, 1e-8), 4))

g = lambda x: x ** 3 - x - 1
print(round(bisection(g, 1, 2, 1e-8), 4))
`,

	solution: `def has_root(f, a, b):
    return f(a) * f(b) < 0

def bisection(f, a, b, tol):
    while b - a >= tol:
        m = (a + b) / 2
        if f(a) * f(m) < 0:
            b = m
        else:
            a = m
    return (a + b) / 2

f = lambda x: x ** 2 - 2  # root is sqrt(2)
print(has_root(f, 1, 2))
print(round(bisection(f, 1, 2, 1e-8), 4))

g = lambda x: x ** 3 - x - 1
print(round(bisection(g, 1, 2, 1e-8), 4))
`,

	tests: [
		{
			name: "x^2 - 2 has a root in [1, 2]",
			code: `{{FUNC}}
f = lambda x: x ** 2 - 2
print(has_root(f, 1, 2))`,
			expected: "True\n",
		},
		{
			name: "bisection finds sqrt(2)",
			code: `{{FUNC}}
f = lambda x: x ** 2 - 2
print(round(bisection(f, 1, 2, 1e-8), 4))`,
			expected: "1.4142\n",
		},
		{
			name: "bisection finds root of x^3 - x - 1",
			code: `{{FUNC}}
g = lambda x: x ** 3 - x - 1
print(round(bisection(g, 1, 2, 1e-8), 4))`,
			expected: "1.3247\n",
		},
		{
			name: "no root when signs agree",
			code: `{{FUNC}}
f = lambda x: x ** 2 + 1
print(has_root(f, 0, 5))`,
			expected: "False\n",
		},
	],
};
