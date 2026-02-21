import type { Lesson } from "../../types";

export const lineIntegral: Lesson = {
	id: "line_integral",
	title: "Line Integral of a Scalar Field",
	chapterId: "vector-calculus",
	content: `## Line Integrals

A **line integral** (or path integral) integrates a scalar field $f(x,y)$ along a curve $C$:

$$\\int_C f(x,y)\\, ds$$

where $ds$ is the arc-length element — each tiny piece of the curve weighted by the function value at that point.

### Parameterization

If the curve is given by $\\mathbf{r}(t) = (x(t), y(t))$ for $t \\in [a, b]$, then:

$$ds = \\sqrt{\\left(\\frac{dx}{dt}\\right)^2 + \\left(\\frac{dy}{dt}\\right)^2}\\, dt$$

So:

$$\\int_C f\\, ds = \\int_a^b f(x(t), y(t)) \\cdot \\sqrt{x'(t)^2 + y'(t)^2}\\, dt$$

### Examples

**Arc length** (set $f = 1$):
$$\\int_C 1\\, ds = \\text{length of } C$$

For a unit circle ($x = \\cos t$, $y = \\sin t$, $t \\in [0, 2\\pi]$):
$$\\int_C 1\\, ds = \\int_0^{2\\pi} 1 \\cdot 1\\, dt = 2\\pi \\approx 6.2832$$

**Integral of $f = x$ along $x$-axis from 0 to 2** ($x=t, y=0, ds=dt$):
$$\\int_0^2 t\\, dt = 2.0000$$

### Numerical Computation

\`\`\`c
double line_integral(double (*f)(double, double),
                     double (*xt)(double), double (*yt)(double),
                     double (*dxt)(double), double (*dyt)(double),
                     double a, double b, int n) {
    double h = (b - a) / n, sum = 0.0;
    for (int i = 0; i < n; i++) {
        double t = a + i * h;
        double dx = dxt(t), dy = dyt(t);
        sum += f(xt(t), yt(t)) * sqrt(dx*dx + dy*dy) * h;
    }
    return sum;
}
\`\`\`

### Your Task

Implement \`double line_integral(f, xt, yt, dxt, dyt, a, b, n)\` using a left Riemann sum over the parameterization.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double line_integral(double (*f)(double, double),
                     double (*xt)(double), double (*yt)(double),
                     double (*dxt)(double), double (*dyt)(double),
                     double a, double b, int n) {
\treturn 0.0;
}

double one(double x, double y) { return 1.0; }
double cx(double t) { return cos(t); }
double cy(double t) { return sin(t); }
double dcx(double t) { return -sin(t); }
double dcy(double t) { return cos(t); }

int main() {
\t/* arc length of unit circle = 2π */
\tprintf("%.4f\\n", line_integral(one, cx, cy, dcx, dcy, 0.0, 2.0*M_PI, 10000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double line_integral(double (*f)(double, double),
                     double (*xt)(double), double (*yt)(double),
                     double (*dxt)(double), double (*dyt)(double),
                     double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble t = a + i * h;
\t\tdouble dx = dxt(t), dy = dyt(t);
\t\tsum += f(xt(t), yt(t)) * sqrt(dx*dx + dy*dy) * h;
\t}
\treturn sum;
}

double one(double x, double y) { return 1.0; }
double cx(double t) { return cos(t); }
double cy(double t) { return sin(t); }
double dcx(double t) { return -sin(t); }
double dcy(double t) { return cos(t); }

int main() {
\tprintf("%.4f\\n", line_integral(one, cx, cy, dcx, dcy, 0.0, 2.0*M_PI, 10000));
\treturn 0;
}
`,

	tests: [
		{
			name: "arc length of unit circle = 2π",
			expected: "6.2832\n",
		},
		{
			name: "arc length of segment [0,0]→[2,0] = 2 (f=1, exact for any n)",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double f(double x, double y)  { return 1.0 + x*0.0 + y*0.0; }
double xt(double t) { return t; }
double yt(double t) { return 0.0 + t*0.0; }
double dxt(double t) { return 1.0 + t*0.0; }
double dyt(double t) { return 0.0 + t*0.0; }
int main() {
\tprintf("%.4f\\n", line_integral(f, xt, yt, dxt, dyt, 0.0, 2.0, 1000));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "arc length of semicircle of radius 3 = 3π",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double circ_one(double x, double y) { return 1.0 + x*0.0 + y*0.0; }
double cx3(double t) { return 3.0*cos(t); }
double cy3(double t) { return 3.0*sin(t); }
double dcx3(double t) { return -3.0*sin(t); }
double dcy3(double t) { return 3.0*cos(t); }
int main() {
\tprintf("%.4f\\n", line_integral(circ_one, cx3, cy3, dcx3, dcy3, 0.0, M_PI, 10000));
\treturn 0;
}`,
			expected: "9.4248\n",
		},
	],
};
