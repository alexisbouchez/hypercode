import type { Lesson } from "../../types";

export const directionalDerivative: Lesson = {
	id: "directional_deriv",
	title: "Directional Derivative",
	chapterId: "partial-derivatives",
	content: `## The Directional Derivative

The **directional derivative** measures the rate of change of $f$ in a specific direction $\\mathbf{u}$:

$$D_{\\mathbf{u}} f(x, y) = \\nabla f \\cdot \\hat{\\mathbf{u}}$$

Where $\\hat{\\mathbf{u}} = (u_x, u_y)$ is a **unit vector** ($|\\hat{\\mathbf{u}}| = 1$).

This is the dot product of the gradient with the direction vector.

### Expanding the Formula

$$D_{\\mathbf{u}} f = \\frac{\\partial f}{\\partial x} u_x + \\frac{\\partial f}{\\partial y} u_y$$

### Why Unit Vectors?

Using a unit vector normalizes the measurement. If we doubled the direction vector, we'd get twice the rate — but we want a pure measure of slope in that direction, independent of how long the direction vector is.

### Special Cases

- $\\mathbf{u} = (1, 0)$: directional derivative $= \\partial f / \\partial x$
- $\\mathbf{u} = (0, 1)$: directional derivative $= \\partial f / \\partial y$
- $\\mathbf{u} = \\nabla f / |\\nabla f|$: maximum directional derivative $= |\\nabla f|$
- $\\mathbf{u} = -\\nabla f / |\\nabla f|$: minimum (most negative) directional derivative $= -|\\nabla f|$

### Example

For $f(x,y) = x^2 + y^2$ at $(1, 1)$, direction $\\mathbf{u} = (\\frac{1}{\\sqrt{2}}, \\frac{1}{\\sqrt{2}})$:
- $\\nabla f = (2, 2)$
- $D_{\\mathbf{u}} f = 2 \\cdot \\frac{1}{\\sqrt{2}} + 2 \\cdot \\frac{1}{\\sqrt{2}} = \\frac{4}{\\sqrt{2}} = 2\\sqrt{2} \\approx 2.8284$

### Your Task

Implement \`double directional_deriv(double (*f)(double, double), double x, double y, double ux, double uy, double h)\` that computes the directional derivative using central differences and the dot product formula.

Note: assume $(u_x, u_y)$ is already a unit vector.`,

	starterCode: `#include <stdio.h>

double directional_deriv(double (*f)(double, double),
                          double x, double y,
                          double ux, double uy, double h) {
\t/* D_u f = fx*ux + fy*uy */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* df in direction (1,0) at (3,4) = df/dx = 6 */
\tprintf("%.4f\\n", directional_deriv(f, 3.0, 4.0, 1.0, 0.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double directional_deriv(double (*f)(double, double),
                          double x, double y,
                          double ux, double uy, double h) {
\tdouble fx = (f(x + h, y) - f(x - h, y)) / (2.0 * h);
\tdouble fy = (f(x, y + h) - f(x, y - h)) / (2.0 * h);
\treturn fx*ux + fy*uy;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", directional_deriv(f, 3.0, 4.0, 1.0, 0.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "D_(1,0) f at (3,4) = ∂f/∂x = 6",
			expected: "6.0000\n",
		},
		{
			name: "D_(0,1) f at (3,4) = ∂f/∂y = 8",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*x + y*y; }
int main() {
\tprintf("%.4f\\n", directional_deriv(g, 3.0, 4.0, 0.0, 1.0, 1e-7));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
		{
			name: "D_(1/√2,1/√2)(x²+y²) at (1,1) = 2√2 ≈ 2.8284",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double g(double x, double y) { return x*x + y*y; }
int main() {
\tdouble inv = 1.0 / sqrt(2.0);
\tprintf("%.4f\\n", directional_deriv(g, 1.0, 1.0, inv, inv, 1e-7));
\treturn 0;
}`,
			expected: "2.8284\n",
		},
		{
			name: "D_(1,0)(3x+4y) = 3 everywhere",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return 3*x + 4*y; }
int main() {
\tprintf("%.4f\\n", directional_deriv(g, 5.0, 7.0, 1.0, 0.0, 1e-7));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
	],
};
