import type { Lesson } from "../../types";

export const discriminant2d: Lesson = {
	id: "discriminant_2d",
	title: "Second Derivative Test",
	chapterId: "optimization",
	content: `## Critical Points and the Second Derivative Test

For a function $f(x,y)$, critical points occur where $\\nabla f = \\mathbf{0}$ (both partial derivatives are zero).

### The Discriminant

At a critical point $(x_0, y_0)$, classify it using:

$$D = f_{xx} \\cdot f_{yy} - (f_{xy})^2$$

Where:
- $f_{xx} = \\partial^2 f / \\partial x^2$
- $f_{yy} = \\partial^2 f / \\partial y^2$
- $f_{xy} = \\partial^2 f / \\partial x \\partial y$ (mixed partial)

### Classification Rules

| $D$ | $f_{xx}$ | Type |
|---|-----|------|
| $D > 0$ | $f_{xx} > 0$ | Local **minimum** |
| $D > 0$ | $f_{xx} < 0$ | Local **maximum** |
| $D < 0$ | — | **Saddle point** |
| $D = 0$ | — | Test **inconclusive** |

### Examples

**$f(x,y) = x^2 + y^2$** at $(0,0)$:
- $f_{xx} = 2$, $f_{yy} = 2$, $f_{xy} = 0$
- $D = 4 - 0 = 4 > 0$, $f_{xx} > 0$ → **local minimum** ✓

**$f(x,y) = x^2 - y^2$** at $(0,0)$:
- $f_{xx} = 2$, $f_{yy} = -2$, $f_{xy} = 0$
- $D = -4 < 0$ → **saddle point** ✓

**$f(x,y) = -(x^2 + y^2)$** at $(0,0)$:
- $f_{xx} = -2$, $f_{yy} = -2$, $f_{xy} = 0$
- $D = 4 > 0$, $f_{xx} < 0$ → **local maximum** ✓

### Computing $f_{xy}$ Numerically

$$f_{xy} \\approx \\frac{f(x+h,y+h) - f(x+h,y-h) - f(x-h,y+h) + f(x-h,y-h)}{4h^2}$$

### Your Task

Implement \`double discriminant_2d(double (*f)(double, double), double x, double y, double h)\` that computes $D = f_{xx} \\cdot f_{yy} - f_{xy}^2$.`,

	starterCode: `#include <stdio.h>

double discriminant_2d(double (*f)(double, double),
                        double x, double y, double h) {
\t/* D = fxx*fyy - fxy^2 */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* D for x^2+y^2 at (0,0) = 4 */
\tprintf("%.4f\\n", discriminant_2d(f, 0.0, 0.0, 1e-4));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double discriminant_2d(double (*f)(double, double),
                        double x, double y, double h) {
\tdouble fxx = (f(x + h, y) - 2.0*f(x, y) + f(x - h, y)) / (h * h);
\tdouble fyy = (f(x, y + h) - 2.0*f(x, y) + f(x, y - h)) / (h * h);
\tdouble fxy = (f(x+h,y+h) - f(x+h,y-h) - f(x-h,y+h) + f(x-h,y-h)) / (4.0*h*h);
\treturn fxx*fyy - fxy*fxy;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", discriminant_2d(f, 0.0, 0.0, 1e-4));
\treturn 0;
}
`,

	tests: [
		{
			name: "D for x²+y² at (0,0) = 4 (local min)",
			expected: "4.0000\n",
		},
		{
			name: "D for x²-y² at (0,0) = -4 (saddle)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*x - y*y; }
int main() {
\tprintf("%.4f\\n", discriminant_2d(g, 0.0, 0.0, 1e-4));
\treturn 0;
}`,
			expected: "-4.0000\n",
		},
		{
			name: "D for -(x²+y²) at (0,0) = 4 (local max)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return -(x*x + y*y); }
int main() {
\tprintf("%.4f\\n", discriminant_2d(g, 0.0, 0.0, 1e-4));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "D for x·y at (0,0) = -1 (saddle)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", discriminant_2d(g, 0.0, 0.0, 1e-4));
\treturn 0;
}`,
			expected: "-1.0000\n",
		},
	],
};
