import type { Lesson } from "../../types";

export const partialY: Lesson = {
	id: "partial_y",
	title: "Partial Derivative ∂f/∂y",
	chapterId: "partial-derivatives",
	content: `## Partial Derivative with Respect to y

Just as $\\partial f / \\partial x$ differentiates with $x$ varying and $y$ fixed, $\\partial f / \\partial y$ differentiates with $y$ varying and $x$ fixed:

$$\\frac{\\partial f}{\\partial y} = \\lim_{h \\to 0} \\frac{f(x,\\, y+h) - f(x,\\, y-h)}{2h}$$

### Symmetry Between Variables

For a "symmetric" function like $f(x,y) = x^2 + y^2$, we get:
- $\\partial f / \\partial x = 2x$
- $\\partial f / \\partial y = 2y$

They have the same form — $x$ and $y$ play identical roles.

### Mixed Partials

Higher-order mixed partials $\\frac{\\partial^2 f}{\\partial x \\partial y} = \\frac{\\partial^2 f}{\\partial y \\partial x}$ for smooth functions (Schwarz's theorem). This symmetry is a powerful fact used throughout multivariable calculus.

### Examples

For $f(x, y) = x^2 y + \\sin(y)$:
- $\\partial f / \\partial y = x^2 + \\cos(y)$

For $f(x, y) = e^x \\cdot y^2$:
- $\\partial f / \\partial y = 2y \\cdot e^x$

For $f(x, y) = x^3 + 3xy^2$:
- $\\partial f / \\partial y = 6xy$

### Your Task

Implement \`double partial_y(double (*f)(double, double), double x, double y, double h)\` that approximates $\\partial f / \\partial y$ at $(x, y)$ using central differences.`,

	starterCode: `#include <stdio.h>

double partial_y(double (*f)(double, double),
                 double x, double y, double h) {
\t/* central difference in y direction */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* df/dy of x^2+y^2 at (3,4) = 2*4 = 8 */
\tprintf("%.4f\\n", partial_y(f, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double partial_y(double (*f)(double, double),
                 double x, double y, double h) {
\treturn (f(x, y + h) - f(x, y - h)) / (2.0 * h);
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", partial_y(f, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "∂(x²+y²)/∂y at (3,4) = 8",
			expected: "8.0000\n",
		},
		{
			name: "∂(x·y)/∂y at (3,4) = 3 (x constant)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", partial_y(g, 3.0, 4.0, 1e-7));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "∂(y³)/∂y at y=2 = 12",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*0 + y*y*y; }
int main() {
\tprintf("%.4f\\n", partial_y(g, 0.0, 2.0, 1e-7));
\treturn 0;
}`,
			expected: "12.0000\n",
		},
		{
			name: "∂(2x+3y)/∂y at any point = 3",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return 2*x + 3*y; }
int main() {
\tprintf("%.4f\\n", partial_y(g, 5.0, 7.0, 1e-7));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
	],
};
