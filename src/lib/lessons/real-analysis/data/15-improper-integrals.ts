import type { Lesson } from "../../types";

export const improperIntegrals: Lesson = {
	id: "improper-integrals",
	title: "Improper Integrals",
	chapterId: "integration",
	content: `## Improper Integrals

An **improper integral** has either an infinite limit of integration or an integrand that is unbounded.

### Type 1: Infinite Limits

$$\\int_a^{\\infty} f(x)\\,dx = \\lim_{t \\to \\infty} \\int_a^t f(x)\\,dx$$

The integral **converges** if this limit exists and is finite.

### Type 2: Unbounded Integrands

If \\(f\\) is unbounded at \\(a\\):

$$\\int_a^b f(x)\\,dx = \\lim_{\\varepsilon \\to 0^+} \\int_{a + \\varepsilon}^b f(x)\\,dx$$

### Key Examples

| Integral | Converges? | Value |
|----------|-----------|-------|
| \\(\\int_1^{\\infty} 1/x^2\\,dx\\) | Yes | 1 |
| \\(\\int_1^{\\infty} 1/x\\,dx\\) | No | \\(\\infty\\) |
| \\(\\int_1^{\\infty} e^{-x}\\,dx\\) | Yes | \\(1/e\\) |

### The p-Test

\\(\\int_1^{\\infty} 1/x^p\\,dx\\) converges if and only if \\(p > 1\\).

### Numerical Approximation

We approximate \\(\\int_a^{\\infty} f(x)\\,dx\\) by computing \\(\\int_a^T f(x)\\,dx\\) for increasingly large \\(T\\) and checking for convergence.

### Your Task

Implement:
1. \`improper_integral(f, a, T, n)\` -- approximates \\(\\int_a^T f(x)\\,dx\\) using Simpson's rule with \\(n\\) subintervals
2. \`test_convergence(f, a, T_values)\` -- computes the integral for each \\(T\\) in \`T_values\` and returns the list of values
3. \`p_test(p, T, n)\` -- evaluates \\(\\int_1^T 1/x^p\\,dx\\) to numerically check the p-test`,

	starterCode: `def improper_integral(f, a, T, n=2000):
    # Simpson's rule for integral from a to T
    pass

def test_convergence(f, a, T_values, n=2000):
    # Return list of integral values for each T
    pass

def p_test(p, T=1000, n=10000):
    # Evaluate integral of 1/x^p from 1 to T
    pass

import math

# integral of 1/x^2 from 1 to infinity = 1
print([round(v, 2) for v in test_convergence(lambda x: 1/x**2, 1, [10, 100, 1000])])

# integral of e^(-x) from 0 to infinity = 1
print([round(v, 2) for v in test_convergence(lambda x: math.exp(-x), 0, [5, 10, 20])])

# p-test: p=2 converges, p=0.5 diverges
print(round(p_test(2), 4))
print(round(p_test(0.5), 4))
`,

	solution: `def improper_integral(f, a, T, n=2000):
    if n % 2 != 0:
        n += 1
    dx = (T - a) / n
    s = f(a) + f(T)
    for i in range(1, n):
        x = a + i * dx
        if i % 2 == 0:
            s += 2 * f(x)
        else:
            s += 4 * f(x)
    return s * dx / 3

def test_convergence(f, a, T_values, n=2000):
    return [improper_integral(f, a, T, n) for T in T_values]

def p_test(p, T=1000, n=10000):
    return improper_integral(lambda x: 1 / x ** p, 1, T, n)

import math

# integral of 1/x^2 from 1 to infinity = 1
print([round(v, 2) for v in test_convergence(lambda x: 1/x**2, 1, [10, 100, 1000])])

# integral of e^(-x) from 0 to infinity = 1
print([round(v, 2) for v in test_convergence(lambda x: math.exp(-x), 0, [5, 10, 20])])

# p-test: p=2 converges, p=0.5 diverges
print(round(p_test(2), 4))
print(round(p_test(0.5), 4))
`,

	tests: [
		{
			name: "1/x^2 integral converges to 1",
			code: `{{FUNC}}
print([round(v, 2) for v in test_convergence(lambda x: 1/x**2, 1, [10, 100, 1000])])`,
			expected: "[0.9, 0.99, 1.0]\n",
		},
		{
			name: "e^(-x) integral converges to 1",
			code: `{{FUNC}}
import math
print([round(v, 2) for v in test_convergence(lambda x: math.exp(-x), 0, [5, 10, 20])])`,
			expected: "[0.99, 1.0, 1.0]\n",
		},
		{
			name: "p-test with p=2 converges near 1",
			code: `{{FUNC}}
print(round(p_test(2), 4))`,
			expected: "0.999\n",
		},
	],
};
