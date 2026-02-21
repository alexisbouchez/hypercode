import type { Lesson } from "../../types";

export const greensTheorem: Lesson = {
	id: "greens_theorem",
	title: "Green's Theorem",
	chapterId: "vector-calculus",
	content: `## Green's Theorem

**Green's theorem** connects a line integral around a closed curve to a double integral over the enclosed region:

$$\\oint_{\\partial R} P\\,dx + Q\\,dy = \\iint_R \\left(\\frac{\\partial Q}{\\partial x} - \\frac{\\partial P}{\\partial y}\\right) dA$$

This is one of the central results of vector calculus — it shows that circulation around the boundary equals the total curl inside.

### Area Formula

A beautiful consequence: if we choose $P = -y/2$, $Q = x/2$, then:

$$\\frac{\\partial Q}{\\partial x} - \\frac{\\partial P}{\\partial y} = \\frac{1}{2} + \\frac{1}{2} = 1$$

So:

$$\\text{Area}(R) = \\frac{1}{2} \\oint_{\\partial R} (-y\\,dx + x\\,dy)$$

### Numerical Verification

We can verify Green's theorem numerically by computing both sides independently:

1. **Double integral side**: integrate the curl $\\partial Q/\\partial x - \\partial P/\\partial y$ over $R$
2. **Line integral side**: integrate $P\\,dx + Q\\,dy$ around the boundary

For $P = -y$, $Q = x$ over the unit square $[0,1]\\times[0,1]$:
- Curl: $\\partial Q/\\partial x - \\partial P/\\partial y = 1 - (-1) = 2$
- Double integral: $2 \\times \\text{Area} = 2 \\times 1 = 2$

### Implementing the Double Integral Side

\`\`\`c
double greens_double_integral(
    double (*curl)(double, double),
    double ax, double bx, double ay, double by, int n) {
    double hx = (bx - ax) / n, hy = (by - ay) / n, sum = 0.0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            sum += curl(ax + i*hx, ay + j*hy) * hx * hy;
    return sum;
}
\`\`\`

### Your Task

Implement \`double greens_double_integral(curl, ax, bx, ay, by, n)\` that numerically integrates a curl function over the rectangle $[ax, bx] \\times [ay, by]$.`,

	starterCode: `#include <stdio.h>

double greens_double_integral(
\tdouble (*curl)(double, double),
\tdouble ax, double bx, double ay, double by, int n) {
\treturn 0.0;
}

/* curl of (-y, x) = dQ/dx - dP/dy = 1 - (-1) = 2 */
double curl_rot(double x, double y) { return 2.0 + x*0.0 + y*0.0; }

int main() {
\t/* 2 * area([0,1]x[0,1]) = 2 */
\tprintf("%.4f\\n", greens_double_integral(curl_rot, 0.0, 1.0, 0.0, 1.0, 100));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double greens_double_integral(
\tdouble (*curl)(double, double),
\tdouble ax, double bx, double ay, double by, int n) {
\tdouble hx = (bx - ax) / n;
\tdouble hy = (by - ay) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tfor (int j = 0; j < n; j++) {
\t\t\tsum += curl(ax + i * hx, ay + j * hy) * hx * hy;
\t\t}
\t}
\treturn sum;
}

double curl_rot(double x, double y) { return 2.0 + x*0.0 + y*0.0; }

int main() {
\tprintf("%.4f\\n", greens_double_integral(curl_rot, 0.0, 1.0, 0.0, 1.0, 100));
\treturn 0;
}
`,

	tests: [
		{
			name: "curl=2 over [0,1]²: integral = 2",
			expected: "2.0000\n",
		},
		{
			name: "curl=1 over [0,2]x[0,3]: integral = 6",
			code: `#include <stdio.h>
{{FUNC}}
double c1(double x, double y) { return 1.0 + x*0.0 + y*0.0; }
int main() {
\tprintf("%.4f\\n", greens_double_integral(c1, 0.0, 2.0, 0.0, 3.0, 100));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
		{
			name: "curl=4 over [0,1]²: integral = 4",
			code: `#include <stdio.h>
{{FUNC}}
double c4(double x, double y) { return 4.0 + x*0.0 + y*0.0; }
int main() {
\tprintf("%.4f\\n", greens_double_integral(c4, 0.0, 1.0, 0.0, 1.0, 100));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "curl=0 over any region: integral = 0",
			code: `#include <stdio.h>
{{FUNC}}
double zero_curl(double x, double y) { return 0.0 + x*0.0 + y*0.0; }
int main() {
\tprintf("%.4f\\n", greens_double_integral(zero_curl, 0.0, 5.0, 0.0, 5.0, 100));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
