import type { Lesson } from "../../types";

export const powerSeries: Lesson = {
	id: "power-series",
	title: "Power Series",
	chapterId: "series",
	content: `## Power Series

A **power series** centered at \\(a\\) is:

$$\\sum_{n=0}^{\\infty} c_n (x - a)^n$$

where \\(c_n\\) are the coefficients.

### Radius of Convergence

Every power series has a **radius of convergence** \\(R\\) such that:
- The series converges absolutely for \\(|x - a| < R\\)
- The series diverges for \\(|x - a| > R\\)

The radius can be found using the ratio test:

$$R = \\lim_{n \\to \\infty} \\left| \\frac{c_n}{c_{n+1}} \\right|$$

### Common Power Series

| Function | Power series | Radius |
|----------|-------------|--------|
| \\(e^x\\) | \\(\\sum x^n / n!\\) | \\(\\infty\\) |
| \\(1/(1-x)\\) | \\(\\sum x^n\\) | 1 |
| \\(\\ln(1+x)\\) | \\(\\sum (-1)^{n+1} x^n / n\\) | 1 |

### Evaluating a Power Series

To evaluate at a point \\(x\\), compute the partial sum:

\`\`\`python
def eval_power_series(coeffs, x, N):
    return sum(c * x**n for n, c in enumerate(coeffs[:N+1]))
\`\`\`

### Your Task

Implement:
1. \`eval_power_series(coeff_func, x, N)\` -- given a function \`coeff_func(n)\` returning the \\(n\\)-th coefficient, evaluate \\(\\sum_{n=0}^{N} c_n \\cdot x^n\\)
2. \`estimate_radius(coeff_func, n)\` -- estimate the radius of convergence as \\(|c_n / c_{n+1}|\\)`,

	starterCode: `def eval_power_series(coeff_func, x, N):
    # Compute sum of coeff_func(n) * x^n for n = 0 to N
    pass

def estimate_radius(coeff_func, n):
    # Return |coeff_func(n) / coeff_func(n+1)|
    pass

import math

# e^x coefficients: 1/n!
exp_coeff = lambda n: 1 / math.factorial(n)

# Evaluate e^1 using 15 terms
print(round(eval_power_series(exp_coeff, 1, 15), 4))

# Geometric series: all coefficients = 1, evaluate at x=0.5
geo_coeff = lambda n: 1
print(round(eval_power_series(geo_coeff, 0.5, 20), 4))

# Radius of geometric series
print(round(estimate_radius(geo_coeff, 100), 4))
`,

	solution: `def eval_power_series(coeff_func, x, N):
    return sum(coeff_func(n) * x ** n for n in range(N + 1))

def estimate_radius(coeff_func, n):
    return abs(coeff_func(n) / coeff_func(n + 1))

import math

# e^x coefficients: 1/n!
exp_coeff = lambda n: 1 / math.factorial(n)

# Evaluate e^1 using 15 terms
print(round(eval_power_series(exp_coeff, 1, 15), 4))

# Geometric series: all coefficients = 1, evaluate at x=0.5
geo_coeff = lambda n: 1
print(round(eval_power_series(geo_coeff, 0.5, 20), 4))

# Radius of geometric series
print(round(estimate_radius(geo_coeff, 100), 4))
`,

	tests: [
		{
			name: "e^1 approximation with 15 terms",
			code: `{{FUNC}}
import math
exp_coeff = lambda n: 1 / math.factorial(n)
print(round(eval_power_series(exp_coeff, 1, 15), 4))`,
			expected: "2.7183\n",
		},
		{
			name: "geometric series at x=0.5 approaches 2",
			code: `{{FUNC}}
geo_coeff = lambda n: 1
print(round(eval_power_series(geo_coeff, 0.5, 20), 4))`,
			expected: "2.0\n",
		},
		{
			name: "radius of geometric series is 1",
			code: `{{FUNC}}
geo_coeff = lambda n: 1
print(round(estimate_radius(geo_coeff, 100), 4))`,
			expected: "1.0\n",
		},
	],
};
