import type { Lesson } from "../../types";

export const monotoneConvergence: Lesson = {
	id: "monotone-convergence",
	title: "Monotone Convergence",
	chapterId: "sequences",
	content: `## Monotone Convergence Theorem

The **Monotone Convergence Theorem** is a fundamental result: every bounded monotone sequence converges.

### Monotone Sequences

A sequence \\((a_n)\\) is:
- **Monotonically increasing** if \\(a_{n+1} \\ge a_n\\) for all \\(n\\)
- **Monotonically decreasing** if \\(a_{n+1} \\le a_n\\) for all \\(n\\)
- **Strictly increasing/decreasing** if the inequalities are strict

### The Theorem

If \\((a_n)\\) is increasing and bounded above, then \\(\\lim_{n \\to \\infty} a_n = \\sup\\{a_n\\}\\).

If \\((a_n)\\) is decreasing and bounded below, then \\(\\lim_{n \\to \\infty} a_n = \\inf\\{a_n\\}\\).

### Example: \\(a_n = 1 - 1/n\\)

This sequence is increasing (each term is larger) and bounded above by 1. By the theorem, it converges to \\(\\sup\\{1 - 1/n\\} = 1\\).

### Numerical Detection

We can check monotonicity by comparing consecutive terms:

\`\`\`python
def is_increasing(f, n):
    return all(f(k+1) >= f(k) for k in range(1, n))
\`\`\`

### Your Task

Implement:
1. \`is_monotone_increasing(f, n)\` -- checks if \\(f(k+1) \\ge f(k)\\) for \\(k = 1, \\ldots, n-1\\)
2. \`is_monotone_decreasing(f, n)\` -- checks if \\(f(k+1) \\le f(k)\\) for \\(k = 1, \\ldots, n-1\\)
3. \`estimate_limit(f, n)\` -- returns \\(f(n)\\) as an estimate of the limit using a large \\(n\\)`,

	starterCode: `def is_monotone_increasing(f, n):
    # Check f(k+1) >= f(k) for k = 1 to n-1
    pass

def is_monotone_decreasing(f, n):
    # Check f(k+1) <= f(k) for k = 1 to n-1
    pass

def estimate_limit(f, n):
    # Return f(n) as an approximation of the limit
    pass

a = lambda n: 1 - 1/n
b = lambda n: 1/n

print(is_monotone_increasing(a, 100))
print(is_monotone_decreasing(b, 100))
print(round(estimate_limit(a, 10000), 4))
`,

	solution: `def is_monotone_increasing(f, n):
    return all(f(k + 1) >= f(k) for k in range(1, n))

def is_monotone_decreasing(f, n):
    return all(f(k + 1) <= f(k) for k in range(1, n))

def estimate_limit(f, n):
    return f(n)

a = lambda n: 1 - 1/n
b = lambda n: 1/n

print(is_monotone_increasing(a, 100))
print(is_monotone_decreasing(b, 100))
print(round(estimate_limit(a, 10000), 4))
`,

	tests: [
		{
			name: "1 - 1/n is monotone increasing",
			code: `{{FUNC}}
a = lambda n: 1 - 1/n
print(is_monotone_increasing(a, 100))`,
			expected: "True\n",
		},
		{
			name: "1/n is monotone decreasing",
			code: `{{FUNC}}
b = lambda n: 1/n
print(is_monotone_decreasing(b, 100))`,
			expected: "True\n",
		},
		{
			name: "1/n is not monotone increasing",
			code: `{{FUNC}}
b = lambda n: 1/n
print(is_monotone_increasing(b, 100))`,
			expected: "False\n",
		},
		{
			name: "estimate_limit of 1 - 1/n approaches 1",
			code: `{{FUNC}}
a = lambda n: 1 - 1/n
print(round(estimate_limit(a, 10000), 4))`,
			expected: "0.9999\n",
		},
	],
};
