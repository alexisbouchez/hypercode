import type { Lesson } from "../../types";

export const polarDoubleIntegral: Lesson = {
	id: "polar_double_integral",
	title: "Polar Double Integral",
	chapterId: "multiple-integrals",
	content: `## Integration in Polar Coordinates

Some regions are naturally described in polar coordinates $(r, \\theta)$ rather than Cartesian $(x, y)$.

### The Polar Jacobian

When converting to polar coordinates $x = r\\cos(\\theta)$, $y = r\\sin(\\theta)$, the area element becomes:

$$dA = r\\, dr\\, d\\theta$$

The extra $r$ factor (the Jacobian) accounts for the stretching near the origin.

### The Formula

$$\\iint_R f(x,y)\\, dA = \\int_a^b \\int_{r_0}^{r_1} f(r\\cos\\theta,\\, r\\sin\\theta) \\cdot r\\, dr\\, d\\theta$$

### Midpoint Rule in Polar Coordinates

Divide $[r_0, r_1]$ into $n_r$ strips and $[a, b]$ into $n_\\theta$ angular sectors:

$$\\approx \\sum_i \\sum_j f(r_i \\cos\\theta_j,\\, r_i \\sin\\theta_j) \\cdot r_i \\cdot \\Delta r \\cdot \\Delta\\theta$$

### Classic Example: Area of a Disk

For a disk of radius $R$:

$$\\int_0^{2\\pi} \\int_0^R r\\, dr\\, d\\theta = 2\\pi \\cdot \\frac{R^2}{2} = \\pi R^2 \\checkmark$$

### Applications

- Area of circles, sectors, rings
- Volume of cylinders and cones
- Integrals with $x^2 + y^2$ in them (naturally round)

### Your Task

Implement \`double polar_double_integral(double (*f)(double, double), double r0, double r1, double a, double b, int nr, int ntheta)\`.

The function $f$ takes Cartesian $(x,y)$ coordinates. Inside, convert $(r, \\theta)$ to $(x, y)$ and include the $r$ factor.

Use \`#include <math.h>\` for \`cos\` and \`sin\`.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double polar_double_integral(double (*f)(double, double),
                              double r0, double r1,
                              double a, double b,
                              int nr, int ntheta) {
\t/* ∫∫ f(r*cos(θ), r*sin(θ)) * r dr dθ */
\treturn 0.0;
}

double one(double x, double y) { return 1.0; }

int main() {
\t/* area of unit disk = pi ≈ 3.1416 */
\tprintf("%.4f\\n", polar_double_integral(one, 0, 1, 0, 2*PI, 200, 200));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double polar_double_integral(double (*f)(double, double),
                              double r0, double r1,
                              double a, double b,
                              int nr, int ntheta) {
\tdouble dr = (r1 - r0) / nr;
\tdouble dtheta = (b - a) / ntheta;
\tdouble sum = 0.0;
\tfor (int i = 0; i < nr; i++) {
\t\tdouble r = r0 + (i + 0.5) * dr;
\t\tfor (int j = 0; j < ntheta; j++) {
\t\t\tdouble theta = a + (j + 0.5) * dtheta;
\t\t\tdouble x = r * cos(theta);
\t\t\tdouble y = r * sin(theta);
\t\t\tsum += f(x, y) * r;
\t\t}
\t}
\treturn sum * dr * dtheta;
}

double one(double x, double y) { return 1.0; }

int main() {
\tprintf("%.4f\\n", polar_double_integral(one, 0, 1, 0, 2*PI, 200, 200));
\treturn 0;
}
`,

	tests: [
		{
			name: "area of unit disk ≈ π ≈ 3.1416",
			expected: "3.1416\n",
		},
		{
			name: "area of disk radius 2 ≈ 4π ≈ 12.5664",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tprintf("%.4f\\n", polar_double_integral(one, 0, 2, 0, 2*PI, 200, 200));
\treturn 0;
}`,
			expected: "12.5664\n",
		},
		{
			name: "area of semicircle radius 1 ≈ π/2 ≈ 1.5708",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tprintf("%.4f\\n", polar_double_integral(one, 0, 1, 0, PI, 200, 200));
\treturn 0;
}`,
			expected: "1.5708\n",
		},
		{
			name: "∬(x²+y²) over unit disk = π/2 ≈ 1.5708",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
double g(double x, double y) { return x*x + y*y; }
int main() {
\tprintf("%.4f\\n", polar_double_integral(g, 0, 1, 0, 2*PI, 200, 200));
\treturn 0;
}`,
			expected: "1.5708\n",
		},
	],
};
