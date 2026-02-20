import type { Lesson } from "../../types";

export const partialY: Lesson = {
	id: "partial_y",
	title: "Partial Derivative ∂f/∂y",
	chapterId: "partial-derivatives",
	content: `## Partial Derivative with Respect to y

Just as ∂f/∂x differentiates with x varying and y fixed, ∂f/∂y differentiates with y varying and x fixed:

\`\`\`
∂f/∂y = lim_{h→0} [f(x, y+h) - f(x, y-h)] / (2h)
\`\`\`

### Symmetry Between Variables

For a "symmetric" function like f(x,y) = x² + y², we get:
- ∂f/∂x = 2x
- ∂f/∂y = 2y

They have the same form — x and y play identical roles.

### Mixed Partials

Higher-order mixed partials ∂²f/∂x∂y = ∂²f/∂y∂x for smooth functions (Schwarz's theorem). This symmetry is a powerful fact used throughout multivariable calculus.

### Examples

For f(x, y) = x²y + sin(y):
- ∂f/∂y = x² + cos(y)

For f(x, y) = e^x · y²:
- ∂f/∂y = 2y · e^x

For f(x, y) = x³ + 3xy²:
- ∂f/∂y = 6xy

### Your Task

Implement \`double partial_y(double (*f)(double, double), double x, double y, double h)\` that approximates ∂f/∂y at (x, y) using central differences.`,

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
