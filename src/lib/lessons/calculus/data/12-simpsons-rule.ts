import type { Lesson } from "../../types";

export const simpsonsRule: Lesson = {
	id: "simpsons-rule",
	title: "Simpson's Rule",
	chapterId: "integration",
	content: `## Simpson's Rule

**Simpson's rule** fits a **parabola** through each pair of subintervals (3 points: left, mid, right) instead of a line (trapezoid).

### Formula

With $n$ subintervals (**$n$ must be even**), $h = \\frac{b-a}{n}$:

$$\\frac{h}{3} \\cdot \\left[f(x_0) + 4f(x_1) + 2f(x_2) + 4f(x_3) + 2f(x_4) + \\cdots + 4f(x_{n-1}) + f(x_n)\\right]$$

Pattern of coefficients: **1, 4, 2, 4, 2, ..., 4, 1**

Odd-indexed points get weight **4**, even-indexed interior points get weight **2**, endpoints get weight **1**.

### Why 1-4-2-4-1?

Each pair of subintervals uses the exact integral of the parabola through 3 points, which equals $\\frac{h}{3}(f(a) + 4f(\\text{mid}) + f(b))$. Combining $\\frac{n}{2}$ such pairs gives the pattern.

### Accuracy

Error is $O(h^4)$ — much better than trapezoid ($O(h^2)$):

$$\\text{Error} \\leq \\frac{(b-a)^5}{180 n^4} \\cdot \\max|f^{(4)}(x)|$$

Simpson's rule is **exact for polynomials of degree $\\leq 3$**.

### Comparison for $\\int_0^1 x^2\\, dx = \\frac{1}{3}$

| $n$ | Trapezoid | Simpson's |
|---|-----------|-----------|
| 2 | 0.375 | **0.3333** (exact!) |
| 4 | 0.344 | **0.3333** (exact!) |

Simpson's rule gives the exact answer for $x^2$ with just $n=2$!

### Your Task

Implement \`double simpson(double (*f)(double), double a, double b, int n)\`. Assume $n$ is even.`,

	starterCode: `#include <stdio.h>

double simpson(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = f(a) + f(b);
\tfor (int i = 1; i < n; i++) {
\t\tsum += (i % 2 == 0 ? 2.0 : 4.0) * f(a + i * h);
\t}
\treturn sum * h / 3.0;
}

double quad(double x) { return x * x; }

int main() {
\t/* x^2 from 0 to 1: Simpson is exact -> 1/3 */
\tprintf("%.4f\\n", simpson(quad, 0.0, 1.0, 2));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double simpson(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = f(a) + f(b);
\tfor (int i = 1; i < n; i++) {
\t\tsum += (i % 2 == 0 ? 2.0 : 4.0) * f(a + i * h);
\t}
\treturn sum * h / 3.0;
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", simpson(quad, 0.0, 1.0, 2));
\treturn 0;
}
`,

	tests: [
		{
			name: "x^2 from 0 to 1 with n=2: exactly 1/3",
			expected: "0.3333\n",
		},
		{
			name: "x^3 from 0 to 2 with n=2: exactly 4",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", simpson(cubic, 0.0, 2.0, 2));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "constant 1 from 0 to 3 with n=6 is 3",
			code: `#include <stdio.h>
{{FUNC}}
double one(double x) { return 1.0; }
int main() {
\tprintf("%.4f\\n", simpson(one, 0.0, 3.0, 6));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "x^2 from 0 to 2 with n=4: exactly 8/3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", simpson(quad, 0.0, 2.0, 4));
\treturn 0;
}`,
			expected: "2.6667\n",
		},
	],
};
