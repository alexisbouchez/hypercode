import type { Lesson } from "../../types";

export const gradientDescent: Lesson = {
	id: "gradient-descent",
	title: "Gradient Descent",
	chapterId: "differentiation-rules",
	content: `## Gradient Descent

**Gradient descent** is the most widely used optimization algorithm in machine learning. It minimizes a function by repeatedly stepping in the direction of the **negative gradient** (steepest descent):

$$x_{n+1} = x_n - \\alpha \\cdot f'(x_n)$$

where $\\alpha$ is the **learning rate** (step size).

### Intuition

Imagine standing on a hilly landscape, trying to reach the lowest point. At each step, you look at the slope (gradient) under your feet and take a step downhill. With a small enough step size, you converge to a local minimum.

### Convergence

For a convex function $f(x) = (x - x^*)^2$, the update rule gives:

$$x_{n+1} = x_n - \\alpha \\cdot 2(x_n - x^*) = (1 - 2\\alpha)(x_n - x^*)+ x^*$$

The error shrinks by factor $|1 - 2\\alpha|$ at each step. For $\\alpha = 0.1$, the factor is $0.8$ — the error halves approximately every 3 steps.

### Example

Minimize $f(x) = (x - 3)^2$, so $f'(x) = 2(x-3)$:

\`\`\`c
double df(double x) { return 2.0 * (x - 3.0); }

double x = 0.0;
for (int i = 0; i < 100; i++) {
    x = x - 0.1 * df(x);
}
printf("%.4f\\n", x);  // 3.0000
\`\`\`

### Choosing the Learning Rate

- **Too large**: diverges (overshoots the minimum)
- **Too small**: converges very slowly
- **Just right**: converges reliably

### Your Task

Implement \`double grad_descent(df, start, lr, steps)\` that performs gradient descent on the function with derivative \`df\`.`,

	starterCode: `#include <stdio.h>

double grad_descent(double (*df)(double), double start,
                    double lr, int steps) {
\t/* x -= lr * df(x) for each step */
\treturn 0.0;
}

double df_quad(double x) { return 2.0 * (x - 3.0); }

int main() {
\t/* minimize (x-3)^2 from x=0 */
\tprintf("%.4f\\n", grad_descent(df_quad, 0.0, 0.1, 100));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double grad_descent(double (*df)(double), double start,
                    double lr, int steps) {
\tdouble x = start;
\tfor (int i = 0; i < steps; i++) {
\t\tx = x - lr * df(x);
\t}
\treturn x;
}

double df_quad(double x) { return 2.0 * (x - 3.0); }

int main() {
\tprintf("%.4f\\n", grad_descent(df_quad, 0.0, 0.1, 100));
\treturn 0;
}
`,

	tests: [
		{
			name: "minimize (x-3)² from x=0 converges to 3",
			expected: "3.0000\n",
		},
		{
			name: "minimize (x+2)² from x=10 converges to -2",
			code: `#include <stdio.h>
{{FUNC}}
double df(double x) { return 2.0 * (x + 2.0); }
int main() {
\tprintf("%.4f\\n", grad_descent(df, 10.0, 0.1, 200));
\treturn 0;
}`,
			expected: "-2.0000\n",
		},
		{
			name: "minimize (x-7)² from x=0 converges to 7",
			code: `#include <stdio.h>
{{FUNC}}
double df(double x) { return 2.0 * (x - 7.0); }
int main() {
\tprintf("%.4f\\n", grad_descent(df, 0.0, 0.1, 200));
\treturn 0;
}`,
			expected: "7.0000\n",
		},
		{
			name: "start at minimum: no movement",
			code: `#include <stdio.h>
{{FUNC}}
double df(double x) { return 2.0 * (x - 5.0); }
int main() {
\tprintf("%.4f\\n", grad_descent(df, 5.0, 0.1, 100));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
	],
};
