import type { Lesson } from "../../types";

export const lhopital: Lesson = {
	id: "lhopital",
	title: "L'Hôpital's Rule",
	chapterId: "differentiation-rules",
	content: `## L'Hôpital's Rule

When a limit has the **indeterminate form** $0/0$ or $\\infty/\\infty$, L'Hôpital's rule lets you evaluate it using derivatives:

$$\\lim_{x \\to a} \\frac{f(x)}{g(x)} = \\lim_{x \\to a} \\frac{f'(x)}{g'(x)}$$

provided the right-hand limit exists and $g'(a) \\neq 0$.

### Classic Examples

$$\\lim_{x \\to 0} \\frac{\\sin x}{x} = \\frac{\\cos 0}{1} = 1$$

$$\\lim_{x \\to 1} \\frac{x^2 - 1}{x - 1} = \\frac{2x}{1}\\bigg|_{x=1} = 2$$

$$\\lim_{x \\to 0} \\frac{e^x - 1}{x} = \\frac{e^0}{1} = 1$$

### Numerical Implementation

Numerically, we approximate $f'(a)$ and $g'(a)$ using central differences with a small $h$:

\`\`\`c
double lhopital(double (*f)(double), double (*g)(double),
                double x0, double h) {
    double df = (f(x0 + h) - f(x0 - h)) / (2.0 * h);
    double dg = (g(x0 + h) - g(x0 - h)) / (2.0 * h);
    return df / dg;
}
\`\`\`

This avoids the $f(a)/g(a)$ $0/0$ problem by working with the derivatives directly.

### When to Apply It

L'Hôpital applies when you have $0/0$ or $\\pm\\infty/\\infty$. It does **not** apply to other forms like $0 \\cdot \\infty$ or $1^\\infty$ directly — those need algebraic manipulation first.

### Your Task

Implement \`double lhopital(f, g, x0, h)\` that returns $f'(x_0) / g'(x_0)$ using central differences.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double lhopital(double (*f)(double), double (*g)(double),
                double x0, double h) {
\t/* f'(x0) / g'(x0) via central differences */
\treturn 0.0;
}

double sinf_(double x) { return sin(x); }
double id(double x)    { return x; }

int main() {
\t/* lim sin(x)/x as x→0 = 1 */
\tprintf("%.4f\\n", lhopital(sinf_, id, 0.0, 1e-6));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double lhopital(double (*f)(double), double (*g)(double),
                double x0, double h) {
\tdouble df = (f(x0 + h) - f(x0 - h)) / (2.0 * h);
\tdouble dg = (g(x0 + h) - g(x0 - h)) / (2.0 * h);
\treturn df / dg;
}

double sinf_(double x) { return sin(x); }
double id(double x)    { return x; }

int main() {
\tprintf("%.4f\\n", lhopital(sinf_, id, 0.0, 1e-6));
\treturn 0;
}
`,

	tests: [
		{
			name: "lim sin(x)/x as x→0 = 1",
			expected: "1.0000\n",
		},
		{
			name: "lim (x²-1)/(x-1) as x→1 = 2",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double f(double x) { return x*x - 1.0; }
double g(double x) { return x - 1.0; }
int main() {
\tprintf("%.4f\\n", lhopital(f, g, 1.0, 1e-6));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "lim (e^x-1)/x as x→0 = 1",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double f(double x) { return exp(x) - 1.0; }
double g(double x) { return x; }
int main() {
\tprintf("%.4f\\n", lhopital(f, g, 0.0, 1e-6));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "lim (x²-4)/(x-2) as x→2 = 4",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double f(double x) { return x*x - 4.0; }
double g(double x) { return x - 2.0; }
int main() {
\tprintf("%.4f\\n", lhopital(f, g, 2.0, 1e-6));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
	],
};
