import type { Lesson } from "../../types";

export const partialX: Lesson = {
	id: "partial_x",
	title: "Partial Derivative ∂f/∂x",
	chapterId: "partial-derivatives",
	content: `## Partial Derivatives

A function of two variables, $f(x, y)$, has two partial derivatives — one for each variable.

### $\\partial f / \\partial x$

The partial derivative with respect to $x$ treats $y$ as a constant and differentiates normally:

$$\\frac{\\partial f}{\\partial x} = \\lim_{h \\to 0} \\frac{f(x+h,\\, y) - f(x-h,\\, y)}{2h}$$

This is the **central difference** approximation — more accurate than a one-sided difference.

### Intuition

- $\\partial f / \\partial x$ measures the rate of change of $f$ as you move in the $x$-direction
- Think of it as the slope of $f$ along the $x$-axis, holding $y$ frozen

### Examples

For $f(x, y) = x^2 + y^2$:
- $\\partial f / \\partial x = 2x$ (treat $y^2$ as constant → derivative is 0)

For $f(x, y) = x \\cdot y$:
- $\\partial f / \\partial x = y$ (treat $y$ as a constant multiplier)

For $f(x, y) = x^3 + 2xy$:
- $\\partial f / \\partial x = 3x^2 + 2y$

### Your Task

Implement \`double partial_x(double (*f)(double, double), double x, double y, double h)\` that approximates $\\partial f / \\partial x$ at $(x, y)$ using step size $h$.`,

	starterCode: `#include <stdio.h>

double partial_x(double (*f)(double, double),
                 double x, double y, double h) {
\t/* central difference in x direction */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* df/dx of x^2+y^2 at (3,4) = 2*3 = 6 */
\tprintf("%.4f\\n", partial_x(f, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double partial_x(double (*f)(double, double),
                 double x, double y, double h) {
\treturn (f(x + h, y) - f(x - h, y)) / (2.0 * h);
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", partial_x(f, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "∂(x²+y²)/∂x at (3,4) = 6",
			expected: "6.0000\n",
		},
		{
			name: "∂(x·y)/∂x at (3,4) = 4 (y constant)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", partial_x(g, 3.0, 4.0, 1e-7));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "∂(x³)/∂x at x=2 = 12",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*x*x + y*0; }
int main() {
\tprintf("%.4f\\n", partial_x(g, 2.0, 0.0, 1e-7));
\treturn 0;
}`,
			expected: "12.0000\n",
		},
		{
			name: "∂(2x+3y)/∂x at any point = 2",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return 2*x + 3*y; }
int main() {
\tprintf("%.4f\\n", partial_x(g, 5.0, 7.0, 1e-7));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
	],
};
