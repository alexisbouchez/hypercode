import type { Lesson } from "../../types";

export const divergence2d: Lesson = {
	id: "divergence_2d",
	title: "Divergence",
	chapterId: "multiple-integrals",
	content: `## Divergence of a 2D Vector Field

A **vector field** $\\mathbf{F}(x,y) = (F_x(x,y),\\; F_y(x,y))$ assigns a vector to every point in the plane.

The **divergence** measures how much the field "spreads out" at a point:

$$\\nabla \\cdot \\mathbf{F} = \\frac{\\partial F_x}{\\partial x} + \\frac{\\partial F_y}{\\partial y}$$

### Interpretation

- $\\nabla \\cdot \\mathbf{F} > 0$: the field is a **source** — fluid flows outward
- $\\nabla \\cdot \\mathbf{F} < 0$: the field is a **sink** — fluid flows inward
- $\\nabla \\cdot \\mathbf{F} = 0$: **incompressible** flow — no net gain or loss

### The Divergence Theorem

In 2D, the divergence theorem relates the integral of divergence over a region to flow across the boundary:

$$\\iint_R \\nabla \\cdot \\mathbf{F}\\, dA = \\oint_{\\partial R} \\mathbf{F} \\cdot \\mathbf{n}\\, ds$$

This is the 2D version of Gauss's law.

### Examples

**$\\mathbf{F} = (x, y)$** (radially outward):
- $\\partial F_x / \\partial x = 1$, $\\partial F_y / \\partial y = 1$
- $\\nabla \\cdot \\mathbf{F} = 2$ (constant — uniform spreading)

**$\\mathbf{F} = (-y, x)$** (rotation):
- $\\partial F_x / \\partial x = 0$, $\\partial F_y / \\partial y = 0$
- $\\nabla \\cdot \\mathbf{F} = 0$ (incompressible rotation — no sources or sinks)

**$\\mathbf{F} = (x^2, y^2)$**:
- $\\partial F_x / \\partial x = 2x$, $\\partial F_y / \\partial y = 2y$
- $\\nabla \\cdot \\mathbf{F} = 2(x+y)$

### Your Task

Implement \`double divergence_2d(double (*Fx)(double, double), double (*Fy)(double, double), double x, double y, double h)\` that computes the divergence using central differences.`,

	starterCode: `#include <stdio.h>

double divergence_2d(double (*Fx)(double, double),
                      double (*Fy)(double, double),
                      double x, double y, double h) {
\t/* dFx/dx + dFy/dy */
\treturn 0.0;
}

double field_x(double x, double y) { return x; }
double field_y(double x, double y) { return y; }

int main() {
\t/* div(x,y) = 2 */
\tprintf("%.4f\\n", divergence_2d(field_x, field_y, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double divergence_2d(double (*Fx)(double, double),
                      double (*Fy)(double, double),
                      double x, double y, double h) {
\tdouble dFx_dx = (Fx(x + h, y) - Fx(x - h, y)) / (2.0 * h);
\tdouble dFy_dy = (Fy(x, y + h) - Fy(x, y - h)) / (2.0 * h);
\treturn dFx_dx + dFy_dy;
}

double field_x(double x, double y) { return x; }
double field_y(double x, double y) { return y; }

int main() {
\tprintf("%.4f\\n", divergence_2d(field_x, field_y, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "div(x, y) = 2 everywhere",
			expected: "2.0000\n",
		},
		{
			name: "div(-y, x) = 0 (rotation field, incompressible)",
			code: `#include <stdio.h>
{{FUNC}}
double fx(double x, double y) { return -y; }
double fy(double x, double y) { return x; }
int main() {
\tprintf("%.4f\\n", divergence_2d(fx, fy, 3.0, 4.0, 1e-7));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "div(x², y²) at (1,2) = 2x+2y = 6",
			code: `#include <stdio.h>
{{FUNC}}
double fx(double x, double y) { return x*x + y*0; }
double fy(double x, double y) { return y*y + x*0; }
int main() {
\tprintf("%.4f\\n", divergence_2d(fx, fy, 1.0, 2.0, 1e-7));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
		{
			name: "div(3x, 4y) = 7 everywhere",
			code: `#include <stdio.h>
{{FUNC}}
double fx(double x, double y) { return 3*x + y*0; }
double fy(double x, double y) { return 4*y + x*0; }
int main() {
\tprintf("%.4f\\n", divergence_2d(fx, fy, 5.0, 7.0, 1e-7));
\treturn 0;
}`,
			expected: "7.0000\n",
		},
	],
};
