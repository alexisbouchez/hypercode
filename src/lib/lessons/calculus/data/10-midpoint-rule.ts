import type { Lesson } from "../../types";

export const midpointRule: Lesson = {
	id: "midpoint-rule",
	title: "Midpoint Rule",
	chapterId: "integration",
	content: `## Midpoint Rule

Use the **midpoint** of each subinterval:

$$\\sum_{i=0}^{n-1} f\\!\\left(a + \\left(i + 0.5\\right) h\\right) \\cdot h$$

\`\`\`
a   a+h  a+2h        b
|----|----|----...----|
 ^    ^    ^
 midpoints used
\`\`\`

### Why Midpoints Are Better

The midpoint rule has $O(h^2)$ error — the same order as the trapezoidal rule, and better than left/right ($O(h)$). Intuitively, the midpoint is the "best representative" of a subinterval because it cancels out the linear error terms from both sides.

### Comparison for $\\int_0^1 x^2\\, dx = \\frac{1}{3}$

| $n$ | Left | Midpoint | Trapezoid | Right |
|---|------|----------|-----------|-------|
| 4 | 0.219 | **0.328** | 0.344 | 0.469 |

The midpoint rule is more accurate than both left and right for the same $n$.

### Exact for Linear Functions

The midpoint rule integrates **linear functions exactly**, regardless of $n$. For $f(x) = 2x + 1$ on $[0, 3]$:
- $\\int_0^3 (2x+1)\\, dx = \\left[x^2+x\\right]_0^3 = 12$
- Midpoint rule with any $n$: $\\sum f(a + (i+0.5)h) \\cdot h = 12$ ✓

### Your Task

Implement \`double riemann_mid(double (*f)(double), double a, double b, int n)\`.`,

	starterCode: `#include <stdio.h>

double riemann_mid(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + (i + 0.5) * h);
\t}
\treturn sum * h;
}

double linear(double x) { return 2.0 * x + 1.0; }

int main() {
\t/* integral of 2x+1 from 0 to 3 = 12 (exact for any n) */
\tprintf("%.4f\\n", riemann_mid(linear, 0.0, 3.0, 3));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double riemann_mid(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + (i + 0.5) * h);
\t}
\treturn sum * h;
}

double linear(double x) { return 2.0 * x + 1.0; }

int main() {
\tprintf("%.4f\\n", riemann_mid(linear, 0.0, 3.0, 3));
\treturn 0;
}
`,

	tests: [
		{
			name: "integral of 2x+1 from 0 to 3 is 12 (exact for any n)",
			expected: "12.0000\n",
		},
		{
			name: "integral of x on [0,2] with n=2: f(0.5)+f(1.5) = 2",
			code: `#include <stdio.h>
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", riemann_mid(id, 0.0, 2.0, 2));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "integral of constant 4 from 0 to 3 is 12",
			code: `#include <stdio.h>
{{FUNC}}
double four(double x) { return 4.0; }
int main() {
\tprintf("%.4f\\n", riemann_mid(four, 0.0, 3.0, 100));
\treturn 0;
}`,
			expected: "12.0000\n",
		},
		{
			name: "x^2 on [0,1] with n=1: f(0.5)=0.25",
			code: `#include <stdio.h>
{{FUNC}}
double quad(double x) { return x * x; }
int main() {
\tprintf("%.4f\\n", riemann_mid(quad, 0.0, 1.0, 1));
\treturn 0;
}`,
			expected: "0.2500\n",
		},
	],
};
