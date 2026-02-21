import type { Lesson } from "../../types";

export const parametricArcLength: Lesson = {
	id: "parametric-arc-length",
	title: "Parametric Arc Length",
	chapterId: "parametric-and-polar",
	content: `## Parametric Arc Length

A **parametric curve** is defined by two functions of a parameter $t$:

$$x = x(t), \quad y = y(t), \quad t \in [t_0, t_1]$$

The arc length is:

$$L = \int_{t_0}^{t_1} \sqrt{[x'(t)]^2 + [y'(t)]^2} \, dt$$

### Why It Works

At parameter $t$, the curve moves $x'dt$ in $x$ and $y'dt$ in $y$. The Pythagorean theorem gives length $\sqrt{x'^2+y'^2}\,dt$.

### Examples

**Diagonal line** $x=t, y=t$ on $[0,1]$: $x'=y'=1$, so $L = \sqrt{2} \approx 1.4142$

**Line with slope** $x=t, y=2t$ on $[0,1]$: $x'=1$, $y'=2$, so $L = \sqrt{5} \approx 2.2361$

**Circle** $x=\cos(t), y=\sin(t)$ on $[0,2\pi]$:

$$x'=-\sin(t),\ y'=\cos(t) \implies \sqrt{\sin^2 t + \cos^2 t} = 1$$
$$L = \int_0^{2\pi} 1 \, dt = 2\pi \approx 6.2832$$

### Numerical Approach

\`\`\`c
for each midpoint t:
    double xp = (x_fn(t+h) - x_fn(t-h)) / (2*h);
    double yp = (y_fn(t+h) - y_fn(t-h)) / (2*h);
    sum += my_sqrt(xp*xp + yp*yp);
\`\`\`

### Your Task

Implement \`double param_arc_length(double (*x_fn)(double), double (*y_fn)(double), double t0, double t1, int n, double h)\`.`,

	starterCode: `#include <stdio.h>

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double param_arc_length(double (*x_fn)(double), double (*y_fn)(double),
\t\tdouble t0, double t1, int n, double h) {
\tdouble dt = (t1 - t0) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble t = t0 + (i + 0.5) * dt;
\t\tdouble xp = (x_fn(t + h) - x_fn(t - h)) / (2.0 * h);
\t\tdouble yp = (y_fn(t + h) - y_fn(t - h)) / (2.0 * h);
\t\tsum += my_sqrt(xp * xp + yp * yp);
\t}
\treturn sum * dt;
}

double lt(double t) { return t; }

int main() {
\t/* x=t, y=t on [0,1]: length = sqrt(2) ~ 1.4142 */
\tprintf("%.4f\\n", param_arc_length(lt, lt, 0.0, 1.0, 100, 1e-5));
\treturn 0;
}`,

	solution: `#include <stdio.h>

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double param_arc_length(double (*x_fn)(double), double (*y_fn)(double),
\t\tdouble t0, double t1, int n, double h) {
\tdouble dt = (t1 - t0) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble t = t0 + (i + 0.5) * dt;
\t\tdouble xp = (x_fn(t + h) - x_fn(t - h)) / (2.0 * h);
\t\tdouble yp = (y_fn(t + h) - y_fn(t - h)) / (2.0 * h);
\t\tsum += my_sqrt(xp * xp + yp * yp);
\t}
\treturn sum * dt;
}

double lt(double t) { return t; }

int main() {
\tprintf("%.4f\\n", param_arc_length(lt, lt, 0.0, 1.0, 100, 1e-5));
\treturn 0;
}`,

	tests: [
		{
			name: "x=t, y=t on [0,1]: length = √2 ≈ 1.4142",
			expected: "1.4142\n",
		},
		{
			name: "x=2t, y=0 on [0,3]: length = 2*3 = 6.0000",
			code: `#include <stdio.h>
{{FUNC}}
double two_t(double t) { return 2.0 * t; }
double zero(double t) { return 0.0; }
int main() {
\tprintf("%.4f\\n", param_arc_length(two_t, zero, 0.0, 3.0, 100, 1e-5));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
		{
			name: "x=t, y=2t on [0,1]: length = √5 ≈ 2.2361",
			code: `#include <stdio.h>
{{FUNC}}
double two_t(double t) { return 2.0 * t; }
int main() {
\tprintf("%.4f\\n", param_arc_length(lt, two_t, 0.0, 1.0, 100, 1e-5));
\treturn 0;
}`,
			expected: "2.2361\n",
		},
		{
			name: "x=3t, y=4t on [0,1]: length = √(9+16) = 5.0000",
			code: `#include <stdio.h>
{{FUNC}}
double three_t(double t) { return 3.0 * t; }
double four_t(double t) { return 4.0 * t; }
int main() {
\tprintf("%.4f\\n", param_arc_length(three_t, four_t, 0.0, 1.0, 100, 1e-5));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
	],
};
