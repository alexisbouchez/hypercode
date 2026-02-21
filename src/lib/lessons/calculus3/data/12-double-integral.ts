import type { Lesson } from "../../types";

export const doubleIntegral: Lesson = {
	id: "double_integral",
	title: "Double Integral",
	chapterId: "multiple-integrals",
	content: `## Double Integrals

A **double integral** integrates a function of two variables over a 2D region:

$$\\iint_R f(x,y)\\, dA$$

Over a rectangular region $[x_a, x_b] \\times [y_a, y_b]$.

### The Midpoint Rule in 2D

Divide the region into $n_x \\times n_y$ small rectangles. Evaluate $f$ at each midpoint:

$$\\iint f\\, dA \\approx \\sum_i \\sum_j f(x_i, y_j) \\cdot \\Delta x \\cdot \\Delta y$$

Where $x_i = x_a + (i + 0.5) \\Delta x$ and $y_j = y_a + (j + 0.5) \\Delta y$.

### Geometric Meaning

- $\\iint 1\\, dA$ = area of the region
- $\\iint f\\, dA$ = **signed volume** between $z = f(x,y)$ and the $xy$-plane

### Examples

$\\iint 1\\, dA$ **over $[0,3] \\times [0,4]$** $= 12$ (area of $3 \\times 4$ rectangle)

$\\iint (x+y)\\, dA$ **over $[0,1] \\times [0,1]$** $= 1$ (by exact integration: $\\frac{1}{2} + \\frac{1}{2}$)

$\\iint x^2 y\\, dA$ **over $[0,2] \\times [0,3]$** $= \\frac{8}{3} \\cdot \\frac{9}{2} = 12$

### Fubini's Theorem

For continuous $f$, you can integrate one variable at a time:

$$\\iint f\\, dA = \\int_{y_a}^{y_b} \\left[ \\int_{x_a}^{x_b} f(x,y)\\, dx \\right] dy$$

### Your Task

Implement \`double double_integral(double (*f)(double, double), double xa, double xb, double ya, double yb, int nx, int ny)\` using the 2D midpoint rule.`,

	starterCode: `#include <stdio.h>

double double_integral(double (*f)(double, double),
                        double xa, double xb,
                        double ya, double yb,
                        int nx, int ny) {
\t/* 2D midpoint rule */
\treturn 0.0;
}

double f(double x, double y) { return 1.0; }

int main() {
\t/* ∬1 dA over [0,3]x[0,4] = 12 */
\tprintf("%.4f\\n", double_integral(f, 0,3, 0,4, 100,100));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double double_integral(double (*f)(double, double),
                        double xa, double xb,
                        double ya, double yb,
                        int nx, int ny) {
\tdouble dx = (xb - xa) / nx;
\tdouble dy = (yb - ya) / ny;
\tdouble sum = 0.0;
\tfor (int i = 0; i < nx; i++) {
\t\tdouble x = xa + (i + 0.5) * dx;
\t\tfor (int j = 0; j < ny; j++) {
\t\t\tdouble y = ya + (j + 0.5) * dy;
\t\t\tsum += f(x, y);
\t\t}
\t}
\treturn sum * dx * dy;
}

double f(double x, double y) { return 1.0; }

int main() {
\tprintf("%.4f\\n", double_integral(f, 0,3, 0,4, 100,100));
\treturn 0;
}
`,

	tests: [
		{
			name: "∬1 dA over [0,3]×[0,4] = 12",
			expected: "12.0000\n",
		},
		{
			name: "∬(x+y) dA over [0,1]×[0,1] = 1",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x + y; }
int main() {
\tprintf("%.4f\\n", double_integral(g, 0,1, 0,1, 200,200));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "∬x dA over [0,2]×[0,3] = 6",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x + y*0; }
int main() {
\tprintf("%.4f\\n", double_integral(g, 0,2, 0,3, 100,100));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
		{
			name: "∬(x·y) dA over [0,2]×[0,2] = 4",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", double_integral(g, 0,2, 0,2, 200,200));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
	],
};
