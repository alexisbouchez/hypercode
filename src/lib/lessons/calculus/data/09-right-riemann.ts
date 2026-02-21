import type { Lesson } from "../../types";

export const rightRiemann: Lesson = {
	id: "right-riemann",
	title: "Right Riemann Sum",
	chapterId: "integration",
	content: `## Right Riemann Sum

Use the **right endpoint** of each subinterval:

$$\\sum_{i=1}^{n} f(a + i \\cdot h) \\cdot h \\quad \\text{where } h = \\frac{b-a}{n}$$

\`\`\`
a    a+h  a+2h       b
|----|----|----|...|
     ^    ^         ^
     right endpoints used
\`\`\`

### Left vs. Right vs. Exact

For $\\int_0^1 x^2\\, dx = \\frac{1}{3}$:

| Method | n=10 | n=100 | n=1000 |
|--------|------|-------|--------|
| Left   | 0.285 | 0.328 | 0.332  |
| Right  | 0.385 | 0.338 | 0.334  |
| Exact  | 0.333 | 0.333 | 0.333  |

The left sum underestimates (for increasing $f$) and the right sum overestimates. The true integral is sandwiched between them:

$$\\text{riemann\\_left}(f, a, b, n) \\leq \\int_a^b f(x)\\, dx \\leq \\text{riemann\\_right}(f, a, b, n)$$

(assuming $f$ is increasing; swap inequality if decreasing)

### Average of Left and Right

The average of the left and right Riemann sums is the **trapezoidal rule** (next lesson), which has $O(h^2)$ error.

### Your Task

Implement \`double riemann_right(double (*f)(double), double a, double b, int n)\`. The loop runs from $i=1$ to $i=n$ (inclusive).`,

	starterCode: `#include <stdio.h>

double riemann_right(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 1; i <= n; i++) {
\t\tsum += f(a + i * h);
\t}
\treturn sum * h;
}

double constant(double x) { return 3.0; }

int main() {
\t/* integral of 3 from 0 to 4 = 12 */
\tprintf("%.4f\\n", riemann_right(constant, 0.0, 4.0, 1000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double riemann_right(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 1; i <= n; i++) {
\t\tsum += f(a + i * h);
\t}
\treturn sum * h;
}

double constant(double x) { return 3.0; }

int main() {
\tprintf("%.4f\\n", riemann_right(constant, 0.0, 4.0, 1000));
\treturn 0;
}
`,

	tests: [
		{
			name: "integral of 3 from 0 to 4 is 12",
			expected: "12.0000\n",
		},
		{
			name: "n=1 right sum uses f(b): f(x)=x on [0,4] gives 4*4=16",
			code: `#include <stdio.h>
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", riemann_right(id, 0.0, 4.0, 1));
\treturn 0;
}`,
			expected: "16.0000\n",
		},
		{
			name: "n=2 right sum for x on [0,2]: h=1, f(1)+f(2)=3",
			code: `#include <stdio.h>
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", riemann_right(id, 0.0, 2.0, 2));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "integral of 2 from 1 to 5 is 8",
			code: `#include <stdio.h>
{{FUNC}}
double two(double x) { return 2.0; }
int main() {
\tprintf("%.4f\\n", riemann_right(two, 1.0, 5.0, 10));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
	],
};
