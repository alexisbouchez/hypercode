import type { Lesson } from "../../types";

export const numericalLimit: Lesson = {
	id: "limit",
	title: "Numerical Limits",
	chapterId: "limits-and-derivatives",
	content: `## Limits

The **limit** is the foundation of calculus. Informally, $\\lim_{x \\to a} f(x) = L$ means: as $x$ gets arbitrarily close to $a$, $f(x)$ gets arbitrarily close to $L$.

### Why Limits Matter

Limits let us deal with quantities we can't compute directly — like the slope of a curve at a single point, or the sum of infinitely many terms.

### Numerical Approximation

We can approximate a limit numerically by evaluating $f$ at points very close to $a$, approaching from both sides:

$$\\lim_{x \\to a} f(x) \\approx \\frac{f(a - h) + f(a + h)}{2}$$

For a smooth function, this central average is very accurate when $h$ is small (e.g., \`h = 1e-7\`).

### Examples

| Function | Point | Limit |
|----------|-------|-------|
| $x^2$ | $x \\to 3$ | $9$ |
| $\\frac{x^2 - 4}{x - 2}$ | $x \\to 2$ | $4$ |
| $\\frac{\\sin x}{x}$ | $x \\to 0$ | $1$ |

The third example is famous: $f(0)$ is undefined (division by zero), but the limit exists.

### Removable Discontinuities

When $f(a)$ is undefined but the limit exists, the point is called a **removable discontinuity**. The function $\\frac{x^2 - 4}{x - 2} = \\frac{(x+2)(x-2)}{x-2} = x + 2$ has a hole at $x = 2$, but the limit is $4$.

### Your Task

Implement \`double limit(double (*f)(double), double x, double h)\` that approximates the limit of $f$ at $x$ using step size $h$.`,

	starterCode: `#include <stdio.h>

double limit(double (*f)(double), double x, double h) {
\treturn (f(x - h) + f(x + h)) / 2.0;
}

double quad(double x) { return x * x; }

int main() {
\t/* limit of x^2 as x -> 3 is 9 */
\tprintf("%.4f\\n", limit(quad, 3.0, 1e-7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double limit(double (*f)(double), double x, double h) {
\treturn (f(x - h) + f(x + h)) / 2.0;
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", limit(quad, 3.0, 1e-7));
\treturn 0;
}
`,

	tests: [
		{
			name: "limit of x^2 at x=3 is 9",
			expected: "9.0000\n",
		},
		{
			name: "limit of x^3 at x=2 is 8",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", limit(cubic, 2.0, 1e-7));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
		{
			name: "removable discontinuity: (x^2-4)/(x-2) at x=2 is 4",
			code: `#include <stdio.h>
{{FUNC}}
double removable(double x) { return (x * x - 4.0) / (x - 2.0); }
int main() {
\tprintf("%.4f\\n", limit(removable, 2.0, 1e-7));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "limit of 3x+1 at x=0 is 1",
			code: `#include <stdio.h>
{{FUNC}}
double linear(double x) { return 3.0 * x + 1.0; }
int main() {
\tprintf("%.4f\\n", limit(linear, 0.0, 1e-7));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
	],
};
