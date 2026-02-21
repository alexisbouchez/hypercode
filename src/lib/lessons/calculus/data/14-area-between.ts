import type { Lesson } from "../../types";

export const areaBetween: Lesson = {
	id: "area-between",
	title: "Area Between Curves",
	chapterId: "integral-applications",
	content: `## Area Between Two Curves

The area between two curves $f(x)$ and $g(x)$ on $[a, b]$ is:

$$A = \\int_a^b |f(x) - g(x)|\\, dx$$

The absolute value handles the case where the curves cross (one is above the other at different points).

### When $f \\geq g$ Everywhere

If $f(x) \\geq g(x)$ throughout $[a, b]$, the area simplifies to:

$$A = \\int_a^b [f(x) - g(x)]\\, dx = \\int_a^b f(x)\\, dx - \\int_a^b g(x)\\, dx$$

### Finding Intersection Points

To split the interval where the curves cross, find $x$ where $f(x) = g(x)$ — this is a root-finding problem.

### Classic Example

Area between $y = x$ and $y = x^2$ on $[0, 1]$:
- $x \\geq x^2$ on $[0, 1]$ since $x - x^2 = x(1-x) \\geq 0$
- Area $= \\int_0^1 (x - x^2)\\, dx = \\left[\\frac{x^2}{2} - \\frac{x^3}{3}\\right]_0^1 = \\frac{1}{2} - \\frac{1}{3} = \\frac{1}{6} \\approx 0.1667$

### Numerically

Use the midpoint rule on $|f(x) - g(x)|$:

\`\`\`c
for (int i = 0; i < n; i++) {
    double x = a + (i + 0.5) * h;
    double diff = f(x) - g(x);
    sum += (diff < 0 ? -diff : diff);  /* abs */
}
\`\`\`

### Your Task

Implement \`double area_between(double (*f)(double), double (*g)(double), double a, double b, int n)\` that returns $\\int_a^b |f(x) - g(x)|\\, dx$.`,

	starterCode: `#include <stdio.h>

double area_between(double (*f)(double), double (*g)(double),
                    double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * h;
\t\tdouble diff = f(x) - g(x);
\t\tsum += (diff < 0.0 ? -diff : diff);
\t}
\treturn sum * h;
}

double id(double x) { return x; }
double zero(double x) { return 0.0; }

int main() {
\t/* area between y=x and y=0 on [0,3] = integral of x = 4.5 */
\tprintf("%.4f\\n", area_between(id, zero, 0.0, 3.0, 10000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double area_between(double (*f)(double), double (*g)(double),
                    double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * h;
\t\tdouble diff = f(x) - g(x);
\t\tsum += (diff < 0.0 ? -diff : diff);
\t}
\treturn sum * h;
}

double id(double x) { return x; }
double zero(double x) { return 0.0; }

int main() {
\tprintf("%.4f\\n", area_between(id, zero, 0.0, 3.0, 10000));
\treturn 0;
}
`,

	tests: [
		{
			name: "area between y=x and y=0 on [0,3] is 4.5",
			expected: "4.5000\n",
		},
		{
			name: "area between y=x and y=x^2 on [0,1] is 1/6",
			code: `#include <stdio.h>
{{FUNC}}
double sq(double x) { return x * x; }
int main() {
\tprintf("%.4f\\n", area_between(id, sq, 0.0, 1.0, 10000));
\treturn 0;
}`,
			expected: "0.1667\n",
		},
		{
			name: "area between y=4 and y=x^2 on [0,2] is 16/3",
			code: `#include <stdio.h>
{{FUNC}}
double four(double x) { return 4.0; }
double sq(double x) { return x * x; }
int main() {
\tprintf("%.4f\\n", area_between(four, sq, 0.0, 2.0, 10000));
\treturn 0;
}`,
			expected: "5.3333\n",
		},
		{
			name: "area between y=x^2 and y=x^3 on [0,1] is 1/12",
			code: `#include <stdio.h>
{{FUNC}}
double sq(double x) { return x * x; }
double cb(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", area_between(sq, cb, 0.0, 1.0, 10000));
\treturn 0;
}`,
			expected: "0.0833\n",
		},
	],
};
