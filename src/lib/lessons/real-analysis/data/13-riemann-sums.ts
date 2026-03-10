import type { Lesson } from "../../types";

export const riemannSums: Lesson = {
	id: "riemann-sums",
	title: "Riemann Sums",
	chapterId: "integration",
	content: `## Riemann Sums

Integration in real analysis is built on Riemann sums -- approximations of the area under a curve using rectangles.

### Definition

A **Riemann sum** for \\(f\\) on \\([a, b]\\) with \\(n\\) subintervals is:

$$S_n = \\sum_{i=1}^{n} f(x_i^*) \\cdot \\Delta x$$

where \\(\\Delta x = (b - a) / n\\) and \\(x_i^*\\) is a sample point in the \\(i\\)-th subinterval.

### Types of Riemann Sums

| Type | Sample point |
|------|-------------|
| Left | \\(x_i^* = a + (i-1) \\Delta x\\) |
| Right | \\(x_i^* = a + i \\Delta x\\) |
| Midpoint | \\(x_i^* = a + (i - 0.5) \\Delta x\\) |

### Convergence

As \\(n \\to \\infty\\), all three types converge to the same value: the **Riemann integral** \\(\\int_a^b f(x)\\,dx\\).

### Example

For \\(f(x) = x^2\\) on \\([0, 1]\\), the exact integral is \\(1/3\\). The left Riemann sum with \\(n = 1000\\) gives approximately \\(0.3332\\).

### Your Task

Implement:
1. \`left_riemann(f, a, b, n)\` -- left Riemann sum
2. \`right_riemann(f, a, b, n)\` -- right Riemann sum
3. \`midpoint_riemann(f, a, b, n)\` -- midpoint Riemann sum`,

	starterCode: `def left_riemann(f, a, b, n):
    # Sum f(a + (i-1)*dx) * dx for i = 1 to n
    pass

def right_riemann(f, a, b, n):
    # Sum f(a + i*dx) * dx for i = 1 to n
    pass

def midpoint_riemann(f, a, b, n):
    # Sum f(a + (i-0.5)*dx) * dx for i = 1 to n
    pass

# Integral of x^2 from 0 to 1 = 1/3
f = lambda x: x ** 2
print(round(left_riemann(f, 0, 1, 10000), 4))
print(round(right_riemann(f, 0, 1, 10000), 4))
print(round(midpoint_riemann(f, 0, 1, 10000), 4))
`,

	solution: `def left_riemann(f, a, b, n):
    dx = (b - a) / n
    return sum(f(a + (i - 1) * dx) * dx for i in range(1, n + 1))

def right_riemann(f, a, b, n):
    dx = (b - a) / n
    return sum(f(a + i * dx) * dx for i in range(1, n + 1))

def midpoint_riemann(f, a, b, n):
    dx = (b - a) / n
    return sum(f(a + (i - 0.5) * dx) * dx for i in range(1, n + 1))

# Integral of x^2 from 0 to 1 = 1/3
f = lambda x: x ** 2
print(round(left_riemann(f, 0, 1, 10000), 4))
print(round(right_riemann(f, 0, 1, 10000), 4))
print(round(midpoint_riemann(f, 0, 1, 10000), 4))
`,

	tests: [
		{
			name: "left Riemann sum of x^2 on [0,1]",
			code: `{{FUNC}}
f = lambda x: x ** 2
print(round(left_riemann(f, 0, 1, 10000), 4))`,
			expected: "0.3333\n",
		},
		{
			name: "right Riemann sum of x^2 on [0,1]",
			code: `{{FUNC}}
f = lambda x: x ** 2
print(round(right_riemann(f, 0, 1, 10000), 4))`,
			expected: "0.3334\n",
		},
		{
			name: "midpoint Riemann sum of x^2 on [0,1]",
			code: `{{FUNC}}
f = lambda x: x ** 2
print(round(midpoint_riemann(f, 0, 1, 10000), 4))`,
			expected: "0.3333\n",
		},
	],
};
