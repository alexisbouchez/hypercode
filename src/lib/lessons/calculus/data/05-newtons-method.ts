import type { Lesson } from "../../types";

export const newtonsMethod: Lesson = {
	id: "newtons-method",
	title: "Newton's Method",
	chapterId: "derivative-applications",
	content: `## Newton's Method

**Newton's method** (also called Newton-Raphson) finds roots of $f(x) = 0$ using derivatives. It is one of the most important applications of calculus in numerical computing.

### Idea

Starting from a guess $x_0$, draw the tangent line at $(x_0, f(x_0))$. Where does the tangent line cross the x-axis? That crossing is a better guess.

$$\\text{Tangent line: } y = f(x_0) + f'(x_0) \\cdot (x - x_0)$$

$$\\text{Set } y = 0: \\quad x_1 = x_0 - \\frac{f(x_0)}{f'(x_0)}$$

Repeat: $x_{n+1} = x_n - \\dfrac{f(x_n)}{f'(x_n)}$

### Convergence

Newton's method has **quadratic convergence**: the number of correct decimal places roughly doubles each iteration. Starting from a decent guess, 10 iterations typically gives machine precision (15+ digits).

### Example: $\\sqrt{2}$

Find the root of $f(x) = x^2 - 2$, so $f'(x) = 2x$:

$$x_0 = 2.0$$
$$x_1 = 2 - \\frac{4-2}{4} = 2 - 0.5 = 1.5$$
$$x_2 = 1.5 - \\frac{2.25-2}{3} = 1.5 - 0.0833 = 1.4167$$
$$x_3 = 1.4167 - \\cdots = 1.41422$$

After 5 steps: $1.41421356$ — 8 correct digits from 3 iterations.

### Pitfalls

- If $f'(x_n) = 0$, the method fails (division by zero)
- A bad initial guess can diverge or cycle
- Works best near a simple root where $f'(x) \\neq 0$

### Your Task

Implement \`double newton(double (*f)(double), double (*df)(double), double x0, int iters)\` that applies Newton's method for \`iters\` iterations.`,

	starterCode: `#include <stdio.h>

double newton(double (*f)(double), double (*df)(double), double x0, int iters) {
\tdouble x = x0;
\tfor (int i = 0; i < iters; i++) {
\t\tx = x - f(x) / df(x);
\t}
\treturn x;
}

double f_sqrt2(double x) { return x * x - 2.0; }
double df_sqrt2(double x) { return 2.0 * x; }

int main() {
\t/* root of x^2 - 2 = 0 is sqrt(2) ~ 1.41421 */
\tprintf("%.5f\\n", newton(f_sqrt2, df_sqrt2, 2.0, 20));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double newton(double (*f)(double), double (*df)(double), double x0, int iters) {
\tdouble x = x0;
\tfor (int i = 0; i < iters; i++) {
\t\tx = x - f(x) / df(x);
\t}
\treturn x;
}

double f_sqrt2(double x) { return x * x - 2.0; }
double df_sqrt2(double x) { return 2.0 * x; }

int main() {
\tprintf("%.5f\\n", newton(f_sqrt2, df_sqrt2, 2.0, 20));
\treturn 0;
}
`,

	tests: [
		{
			name: "Newton finds sqrt(2) from x0=2",
			expected: "1.41421\n",
		},
		{
			name: "root of x^2 - 9 is 3",
			code: `#include <stdio.h>
{{FUNC}}
double f9(double x) { return x * x - 9.0; }
double df9(double x) { return 2.0 * x; }
int main() {
\tprintf("%.5f\\n", newton(f9, df9, 4.0, 20));
\treturn 0;
}`,
			expected: "3.00000\n",
		},
		{
			name: "root of x^3 - 8 is 2",
			code: `#include <stdio.h>
{{FUNC}}
double fc(double x) { return x * x * x - 8.0; }
double dfc(double x) { return 3.0 * x * x; }
int main() {
\tprintf("%.5f\\n", newton(fc, dfc, 3.0, 20));
\treturn 0;
}`,
			expected: "2.00000\n",
		},
		{
			name: "one Newton step for linear f=x-1 from x0=100 converges immediately",
			code: `#include <stdio.h>
{{FUNC}}
double fl(double x) { return x - 1.0; }
double dfl(double x) { return 1.0; }
int main() {
\tprintf("%.5f\\n", newton(fl, dfl, 100.0, 1));
\treturn 0;
}`,
			expected: "1.00000\n",
		},
	],
};
