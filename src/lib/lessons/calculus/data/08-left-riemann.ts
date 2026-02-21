import type { Lesson } from "../../types";

export const leftRiemann: Lesson = {
	id: "left-riemann",
	title: "Left Riemann Sum",
	chapterId: "integration",
	content: `## The Definite Integral

The **definite integral** $\\int_a^b f(x)\\, dx$ is the signed area between $f$ and the x-axis from $a$ to $b$.

It is defined as the limit of **Riemann sums** — sums of rectangle areas:

$$\\int_a^b f(x)\\, dx = \\lim_{n \\to \\infty} \\sum_{i} f(x_i) \\cdot \\Delta x$$

where $\\Delta x = \\frac{b-a}{n}$ and $x_i$ is some point in the $i$-th subinterval.

### Left Riemann Sum

Use the **left endpoint** of each subinterval:

$$\\sum_{i=0}^{n-1} f(a + i \\cdot h) \\cdot h \\quad \\text{where } h = \\frac{b-a}{n}$$

\`\`\`
a    a+h  a+2h       b
|----|----|----...---|
^    ^    ^
left endpoints used
\`\`\`

### Accuracy

For an increasing function, the left sum **underestimates** the integral (the rectangles don't reach the curve). For a decreasing function, it **overestimates**. Error is $O(h)$ — halving $n$ halves the error.

### Example

$\\int_0^1 x^2\\, dx$ with $n=4$, $h=0.25$:
- $0.25 \\cdot (f(0) + f(0.25) + f(0.5) + f(0.75))$
- $= 0.25 \\cdot (0 + 0.0625 + 0.25 + 0.5625)$
- $= 0.25 \\cdot 0.875 = 0.21875$
- Exact: $\\frac{1}{3} \\approx 0.3333$ — the left sum underestimates

### Your Task

Implement \`double riemann_left(double (*f)(double), double a, double b, int n)\`.`,

	starterCode: `#include <stdio.h>

double riemann_left(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + i * h);
\t}
\treturn sum * h;
}

double constant(double x) { return 3.0; }

int main() {
\t/* integral of 3 from 0 to 4 = 12 */
\tprintf("%.4f\\n", riemann_left(constant, 0.0, 4.0, 1000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double riemann_left(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + i * h);
\t}
\treturn sum * h;
}

double constant(double x) { return 3.0; }

int main() {
\tprintf("%.4f\\n", riemann_left(constant, 0.0, 4.0, 1000));
\treturn 0;
}
`,

	tests: [
		{
			name: "integral of 3 from 0 to 4 is 12",
			expected: "12.0000\n",
		},
		{
			name: "integral of 1 from 0 to 5 is 5",
			code: `#include <stdio.h>
{{FUNC}}
double one(double x) { return 1.0; }
int main() {
\tprintf("%.4f\\n", riemann_left(one, 0.0, 5.0, 1000));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
		{
			name: "n=1 left sum uses f(a): f(x)=x on [0,4] gives 0*4=0",
			code: `#include <stdio.h>
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", riemann_left(id, 0.0, 4.0, 1));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "n=2 left sum for x on [0,2]: h=1, f(0)+f(1) = 1",
			code: `#include <stdio.h>
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", riemann_left(id, 0.0, 2.0, 2));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
	],
};
