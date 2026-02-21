import type { Lesson } from "../../types";

export const gradientMagnitude: Lesson = {
	id: "gradient_magnitude",
	title: "Gradient Magnitude",
	chapterId: "partial-derivatives",
	content: `## The Gradient

The **gradient** of $f(x, y)$ is the vector of its partial derivatives:

$$\\nabla f = \\left(\\frac{\\partial f}{\\partial x},\\; \\frac{\\partial f}{\\partial y}\\right)$$

### The Gradient Points Uphill

The gradient vector always points in the **direction of steepest ascent**. Its magnitude tells you how steep that ascent is.

$$|\\nabla f| = \\sqrt{\\left(\\frac{\\partial f}{\\partial x}\\right)^2 + \\left(\\frac{\\partial f}{\\partial y}\\right)^2}$$

### Key Facts

- $\\nabla f = \\mathbf{0}$ at critical points (local minima, maxima, saddle points)
- The gradient is perpendicular to level curves (contour lines)
- Moving **against** the gradient is steepest descent — the basis of gradient descent in machine learning

### Examples

For $f(x,y) = x^2 + y^2$:
- $\\nabla f = (2x,\\; 2y)$
- At $(3, 4)$: $|\\nabla f| = \\sqrt{36 + 64} = \\sqrt{100} = 10$

For $f(x,y) = xy$:
- $\\nabla f = (y,\\; x)$
- At $(1, 1)$: $|\\nabla f| = \\sqrt{1 + 1} = \\sqrt{2} \\approx 1.4142$

### Your Task

Implement \`double gradient_magnitude(double (*f)(double, double), double x, double y, double h)\` that computes $|\\nabla f|$ at $(x, y)$ using central differences for the partial derivatives.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double gradient_magnitude(double (*f)(double, double),
                           double x, double y, double h) {
\t/* compute |∇f| = sqrt(fx² + fy²) */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* |∇(x²+y²)| at (3,4) = sqrt(36+64) = 10 */
\tprintf("%.4f\\n", gradient_magnitude(f, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double gradient_magnitude(double (*f)(double, double),
                           double x, double y, double h) {
\tdouble fx = (f(x + h, y) - f(x - h, y)) / (2.0 * h);
\tdouble fy = (f(x, y + h) - f(x, y - h)) / (2.0 * h);
\treturn sqrt(fx*fx + fy*fy);
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", gradient_magnitude(f, 3.0, 4.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "|∇(x²+y²)| at (3,4) = 10",
			expected: "10.0000\n",
		},
		{
			name: "|∇(xy)| at (1,1) = sqrt(2) ≈ 1.4142",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", gradient_magnitude(g, 1.0, 1.0, 1e-7));
\treturn 0;
}`,
			expected: "1.4142\n",
		},
		{
			name: "|∇(x²+y²)| at (0,0) = 0 (critical point)",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double g(double x, double y) { return x*x + y*y; }
int main() {
\tprintf("%.4f\\n", gradient_magnitude(g, 0.0, 0.0, 1e-7));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "|∇(3x+4y)| = 5 everywhere (constant gradient)",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double g(double x, double y) { return 3*x + 4*y; }
int main() {
\tprintf("%.4f\\n", gradient_magnitude(g, 10.0, 20.0, 1e-7));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
	],
};
