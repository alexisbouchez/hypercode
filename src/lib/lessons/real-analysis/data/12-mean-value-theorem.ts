import type { Lesson } from "../../types";

export const meanValueTheorem: Lesson = {
	id: "mean-value-theorem",
	title: "Mean Value Theorem",
	chapterId: "continuity",
	content: `## Mean Value Theorem (MVT)

The MVT connects the derivative of a function to its average rate of change.

### Statement

If \\(f\\) is continuous on \\([a, b]\\) and differentiable on \\((a, b)\\), then there exists \\(c \\in (a, b)\\) such that:

$$f'(c) = \\frac{f(b) - f(a)}{b - a}$$

### Interpretation

There is at least one point where the instantaneous rate of change equals the average rate of change over the interval.

### Applications

- If \\(f'(x) = 0\\) for all \\(x\\) in an interval, then \\(f\\) is constant
- If \\(f'(x) > 0\\) for all \\(x\\), then \\(f\\) is strictly increasing
- **Rolle's Theorem** (special case): if \\(f(a) = f(b)\\), then \\(f'(c) = 0\\) for some \\(c \\in (a, b)\\)

### Finding the MVT Point Numerically

We can search for the point \\(c\\) where the derivative equals the average slope:

\`\`\`python
def find_mvt_point(f, a, b, n=10000, h=1e-7):
    avg_slope = (f(b) - f(a)) / (b - a)
    best_c = a
    best_diff = float('inf')
    for i in range(1, n):
        c = a + (b - a) * i / n
        deriv = (f(c + h) - f(c - h)) / (2 * h)
        diff = abs(deriv - avg_slope)
        if diff < best_diff:
            best_diff = diff
            best_c = c
    return best_c
\`\`\`

### Your Task

Implement:
1. \`average_slope(f, a, b)\` -- returns \\((f(b) - f(a)) / (b - a)\\)
2. \`find_mvt_point(f, a, b, n)\` -- searches \\(n\\) evenly spaced interior points for the one where the numerical derivative is closest to the average slope. Uses central difference with \\(h = 10^{-7}\\).`,

	starterCode: `def average_slope(f, a, b):
    # Return (f(b) - f(a)) / (b - a)
    pass

def find_mvt_point(f, a, b, n=10000):
    # Search n evenly spaced points in (a, b) for the MVT point
    # Use central difference with h=1e-7
    pass

import math

# x^2 on [0, 2]: average slope = 2, f'(c) = 2c = 2 => c = 1
print(round(average_slope(lambda x: x**2, 0, 2), 4))
print(round(find_mvt_point(lambda x: x**2, 0, 2), 4))

# sin(x) on [0, pi]: average slope = 0, cos(c) = 0 => c = pi/2
print(round(find_mvt_point(math.sin, 0, math.pi), 4))
`,

	solution: `def average_slope(f, a, b):
    return (f(b) - f(a)) / (b - a)

def find_mvt_point(f, a, b, n=10000):
    avg = average_slope(f, a, b)
    h = 1e-7
    best_c = a
    best_diff = float('inf')
    for i in range(1, n):
        c = a + (b - a) * i / n
        deriv = (f(c + h) - f(c - h)) / (2 * h)
        diff = abs(deriv - avg)
        if diff < best_diff:
            best_diff = diff
            best_c = c
    return best_c

import math

# x^2 on [0, 2]: average slope = 2, f'(c) = 2c = 2 => c = 1
print(round(average_slope(lambda x: x**2, 0, 2), 4))
print(round(find_mvt_point(lambda x: x**2, 0, 2), 4))

# sin(x) on [0, pi]: average slope = 0, cos(c) = 0 => c = pi/2
print(round(find_mvt_point(math.sin, 0, math.pi), 4))
`,

	tests: [
		{
			name: "average slope of x^2 on [0, 2] is 2",
			code: `{{FUNC}}
print(round(average_slope(lambda x: x**2, 0, 2), 4))`,
			expected: "2.0\n",
		},
		{
			name: "MVT point for x^2 on [0, 2] is c=1",
			code: `{{FUNC}}
print(round(find_mvt_point(lambda x: x**2, 0, 2), 4))`,
			expected: "1.0\n",
		},
		{
			name: "MVT point for sin on [0, pi] is pi/2",
			code: `{{FUNC}}
import math
print(round(find_mvt_point(math.sin, 0, math.pi), 4))`,
			expected: "1.5708\n",
		},
	],
};
