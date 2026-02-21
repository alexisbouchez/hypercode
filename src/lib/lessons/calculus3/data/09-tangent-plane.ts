import type { Lesson } from "../../types";

export const tangentPlane: Lesson = {
	id: "tangent_plane",
	title: "Tangent Plane",
	chapterId: "optimization",
	content: `## The Tangent Plane

Just as a tangent line approximates a 1D curve near a point, a **tangent plane** approximates a 2D surface near a point $(x_0, y_0)$:

$$L(x, y) = f(x_0, y_0) + f_x(x_0, y_0)(x - x_0) + f_y(x_0, y_0)(y - y_0)$$

This is called the **linear approximation** or **linearization** of $f$ at $(x_0, y_0)$.

### Why It Works

Near $(x_0, y_0)$, $f$ changes approximately linearly:
- Moving $\\Delta x$ in the $x$-direction changes $f$ by $\\approx f_x \\cdot \\Delta x$
- Moving $\\Delta y$ in the $y$-direction changes $f$ by $\\approx f_y \\cdot \\Delta y$

The tangent plane captures both effects simultaneously.

### Error of Approximation

The error $|f(x,y) - L(x,y)|$ is small near $(x_0, y_0)$ — it's $O(\\|(x,y)-(x_0,y_0)\\|^2)$ for smooth functions.

### Examples

For $f(x,y) = x^2 + y^2$ at $(1, 2)$:
- $f_x = 2$, $f_y = 4$, $f(1,2) = 5$
- $L(x,y) = 5 + 2(x-1) + 4(y-2)$
- $L(1.1,\\; 2.1) \\approx 5 + 0.2 + 0.4 = 5.6$ (exact: $1.21+4.41=5.62$)

### Your Task

Implement \`double tangent_plane(double (*f)(double, double), double x0, double y0, double x, double y, double h)\` that evaluates the linear approximation $L(x, y)$ at the given $(x, y)$ point, using partial derivatives computed with step size $h$.`,

	starterCode: `#include <stdio.h>

double tangent_plane(double (*f)(double, double),
                     double x0, double y0,
                     double x, double y, double h) {
\t/* L(x,y) = f(x0,y0) + fx*(x-x0) + fy*(y-y0) */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* tangent plane to x^2+y^2 at (1,2), evaluated at (1,2) = exact = 5 */
\tprintf("%.4f\\n", tangent_plane(f, 1.0, 2.0, 1.0, 2.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double tangent_plane(double (*f)(double, double),
                     double x0, double y0,
                     double x, double y, double h) {
\tdouble fx = (f(x0 + h, y0) - f(x0 - h, y0)) / (2.0 * h);
\tdouble fy = (f(x0, y0 + h) - f(x0, y0 - h)) / (2.0 * h);
\treturn f(x0, y0) + fx*(x - x0) + fy*(y - y0);
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", tangent_plane(f, 1.0, 2.0, 1.0, 2.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "tangent plane at (1,2), evaluated at (1,2) = f(1,2) = 5",
			expected: "5.0000\n",
		},
		{
			name: "tangent plane of x²+y² at (0,0), evaluated at (1,1) = 0 (flat at origin)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*x + y*y; }
int main() {
\tprintf("%.4f\\n", tangent_plane(g, 0.0, 0.0, 1.0, 1.0, 1e-7));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "tangent plane of linear f = exact everywhere: 3x+4y at (1,1) evaluated at (2,3) = 18",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return 3*x + 4*y; }
int main() {
\tprintf("%.4f\\n", tangent_plane(g, 1.0, 1.0, 2.0, 3.0, 1e-7));
\treturn 0;
}`,
			expected: "18.0000\n",
		},
		{
			name: "tangent plane of x·y at (2,3) evaluated at (2,3) = 6",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", tangent_plane(g, 2.0, 3.0, 2.0, 3.0, 1e-7));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
	],
};
