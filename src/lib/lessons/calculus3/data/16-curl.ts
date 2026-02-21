import type { Lesson } from "../../types";

export const curl2d: Lesson = {
	id: "curl_2d",
	title: "Curl of a 2D Vector Field",
	chapterId: "vector-calculus",
	content: `## Curl in 2D

For a 2D vector field $\\mathbf{F}(x,y) = (F_x(x,y),\\, F_y(x,y))$, the **curl** (scalar curl, or $z$-component of the 3D curl) measures rotation:

$$\\text{curl}\\,\\mathbf{F} = \\frac{\\partial F_y}{\\partial x} - \\frac{\\partial F_x}{\\partial y}$$

### Interpretation

- $\\text{curl} > 0$: counterclockwise rotation (like a whirlpool)
- $\\text{curl} < 0$: clockwise rotation
- $\\text{curl} = 0$: **irrotational** — no net rotation (potential fields, conservative fields)

### Examples

**$\\mathbf{F} = (-y, x)$** (perfect rotation field):
- $\\partial F_y/\\partial x = 1$, $\\partial F_x/\\partial y = -1$
- $\\text{curl} = 1 - (-1) = 2$ (constant, everywhere)

**$\\mathbf{F} = (x, y)$** (radial outward field):
- $\\partial F_y/\\partial x = 0$, $\\partial F_x/\\partial y = 0$
- $\\text{curl} = 0$ (irrotational — no rotation, just expansion)

**$\\mathbf{F} = (y^2, x^2)$** at $(1, 1)$:
- $\\partial F_y/\\partial x = 2x = 2$, $\\partial F_x/\\partial y = 2y = 2$
- $\\text{curl} = 2 - 2 = 0$

### Stokes' Theorem (2D)

The curl is related to circulation via **Green's theorem**:

$$\\oint_{\\partial R} \\mathbf{F} \\cdot d\\mathbf{r} = \\iint_R \\text{curl}\\,\\mathbf{F} \\, dA$$

### Numerical Implementation

Use central differences to approximate the partial derivatives:

\`\`\`c
double curl_2d(double (*Fx)(double, double), double (*Fy)(double, double),
               double x, double y, double h) {
    double dFy_dx = (Fy(x+h,y) - Fy(x-h,y)) / (2.0*h);
    double dFx_dy = (Fx(x,y+h) - Fx(x,y-h)) / (2.0*h);
    return dFy_dx - dFx_dy;
}
\`\`\`

### Your Task

Implement \`double curl_2d(Fx, Fy, x, y, h)\` using central differences.`,

	starterCode: `#include <stdio.h>

double curl_2d(double (*Fx)(double, double),
               double (*Fy)(double, double),
               double x, double y, double h) {
\t/* dFy/dx - dFx/dy */
\treturn 0.0;
}

double rot_x(double x, double y) { return -y; }
double rot_y(double x, double y) { return  x; }

int main() {
\t/* curl(-y, x) = 2 everywhere */
\tprintf("%.4f\\n", curl_2d(rot_x, rot_y, 1.0, 2.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double curl_2d(double (*Fx)(double, double),
               double (*Fy)(double, double),
               double x, double y, double h) {
\tdouble dFy_dx = (Fy(x + h, y) - Fy(x - h, y)) / (2.0 * h);
\tdouble dFx_dy = (Fx(x, y + h) - Fx(x, y - h)) / (2.0 * h);
\treturn dFy_dx - dFx_dy;
}

double rot_x(double x, double y) { return -y; }
double rot_y(double x, double y) { return  x; }

int main() {
\tprintf("%.4f\\n", curl_2d(rot_x, rot_y, 1.0, 2.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "curl(-y, x) = 2 (rotation field)",
			expected: "2.0000\n",
		},
		{
			name: "curl(x, y) = 0 (radial field, irrotational)",
			code: `#include <stdio.h>
{{FUNC}}
double fx(double x, double y) { return x; }
double fy(double x, double y) { return y; }
int main() {
\tprintf("%.4f\\n", curl_2d(fx, fy, 1.0, 1.0, 1e-7));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "curl(-2y, 3x) = 3-(-2) = 5",
			code: `#include <stdio.h>
{{FUNC}}
double fx(double x, double y) { return -2.0*y + x*0; }
double fy(double x, double y) { return  3.0*x + y*0; }
int main() {
\tprintf("%.4f\\n", curl_2d(fx, fy, 2.0, 3.0, 1e-7));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
		{
			name: "curl(y², x²) at (1,1) = 2x-2y = 0",
			code: `#include <stdio.h>
{{FUNC}}
double fx(double x, double y) { return y*y + x*0; }
double fy(double x, double y) { return x*x + y*0; }
int main() {
\tprintf("%.4f\\n", curl_2d(fx, fy, 1.0, 1.0, 1e-7));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
