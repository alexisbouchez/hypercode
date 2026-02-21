import type { Lesson } from "../../types";

export const criticalPoints: Lesson = {
	id: "critical-points",
	title: "Critical Points",
	chapterId: "derivative-applications",
	content: `## Critical Points

A **critical point** is where \`f'(x) = 0\` or \`f'(x)\` is undefined. Critical points are candidates for local maxima and minima.

### Finding Critical Points Numerically

We use **bisection** on \`f'(x)\`: if \`f'(a) < 0\` and \`f'(b) > 0\` (or vice versa), by the Intermediate Value Theorem there must be a zero of \`f'\` between \`a\` and \`b\`.

\`\`\`
while (b - a > tolerance):
    m = (a + b) / 2
    if f'(a) and f'(m) have the same sign:
        a = m
    else:
        b = m
return (a + b) / 2
\`\`\`

### Classifying Critical Points

Once we find \`c\` where \`f'(c) = 0\`:

| Second derivative | Classification |
|-------------------|---------------|
| \`f''(c) > 0\` | Local **minimum** (concave up) |
| \`f''(c) < 0\` | Local **maximum** (concave down) |
| \`f''(c) = 0\` | Inconclusive (use first derivative test) |

### Example

For \`f(x) = x² - 4x + 3\`:
- \`f'(x) = 2x - 4 = 0\` → \`x = 2\`
- \`f''(2) = 2 > 0\` → local minimum
- \`f(2) = 4 - 8 + 3 = -1\` is the minimum value

### Global Extrema

On a closed interval \`[a, b]\`, the global max/min occurs either at a critical point or at an endpoint. Evaluate \`f\` at all critical points and both endpoints, take the largest/smallest.

### Your Task

Implement \`double critical_point(double (*f)(double), double a, double b, int n, double h)\` that uses bisection on \`f'\` to find a critical point in \`[a, b]\`. Run \`n\` bisection steps; \`h\` is the step for numerical differentiation.`,

	starterCode: `#include <stdio.h>

double critical_point(double (*f)(double), double a, double b, int n, double h) {
\tdouble fa = (f(a + h) - f(a - h)) / (2.0 * h);
\tfor (int i = 0; i < n; i++) {
\t\tdouble m = (a + b) / 2.0;
\t\tdouble fm = (f(m + h) - f(m - h)) / (2.0 * h);
\t\tif (fm == 0.0) { a = b = m; break; }
\t\tif (fa * fm < 0.0) { b = m; }
\t\telse { a = m; fa = fm; }
\t}
\treturn (a + b) / 2.0;
}

double parabola(double x) { return x * x; }

int main() {
\t/* x^2 has critical point (minimum) at x=0 */
\tprintf("%.4f\\n", critical_point(parabola, -2.0, 2.0, 60, 1e-6));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double critical_point(double (*f)(double), double a, double b, int n, double h) {
\tdouble fa = (f(a + h) - f(a - h)) / (2.0 * h);
\tfor (int i = 0; i < n; i++) {
\t\tdouble m = (a + b) / 2.0;
\t\tdouble fm = (f(m + h) - f(m - h)) / (2.0 * h);
\t\tif (fm == 0.0) { a = b = m; break; }
\t\tif (fa * fm < 0.0) { b = m; }
\t\telse { a = m; fa = fm; }
\t}
\treturn (a + b) / 2.0;
}

double parabola(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", critical_point(parabola, -2.0, 2.0, 60, 1e-6));
\treturn 0;
}
`,

	tests: [
		{
			name: "x^2 has critical point at x=0",
			expected: "0.0000\n",
		},
		{
			name: "(x-3)^2 has critical point at x=3",
			code: `#include <stdio.h>
{{FUNC}}
double shifted(double x) { return (x - 3.0) * (x - 3.0); }
int main() {
\tprintf("%.4f\\n", critical_point(shifted, 0.0, 6.0, 60, 1e-6));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "x^3 - 3x has critical point at x=1 in [0,2]",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x - 3.0 * x; }
int main() {
\tprintf("%.4f\\n", critical_point(cubic, 0.0, 2.0, 60, 1e-6));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "-(x-2)^2 has critical point at x=2",
			code: `#include <stdio.h>
{{FUNC}}
double inverted(double x) { return -(x - 2.0) * (x - 2.0); }
int main() {
\tprintf("%.4f\\n", critical_point(inverted, 0.0, 4.0, 60, 1e-6));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
	],
};
