import type { Lesson } from "../../types";

export const averageValue: Lesson = {
	id: "average-value",
	title: "Average Value of a Function",
	chapterId: "integral-applications",
	content: `## Average Value of a Function

The **average value** of \`f\` on \`[a, b]\` is:

\`\`\`
f_avg = (1/(b-a)) · ∫_a^b f(x) dx
\`\`\`

This generalizes the discrete average \`(f₁ + f₂ + ... + fₙ)/n\` to a continuous function.

### Mean Value Theorem for Integrals

If \`f\` is continuous on \`[a, b]\`, there exists \`c ∈ [a, b]\` such that:

\`\`\`
f(c) = f_avg = (1/(b-a)) · ∫_a^b f(x) dx
\`\`\`

In other words, \`f\` actually achieves its average value somewhere in the interval.

### Geometric Interpretation

The average value is the height of a rectangle with base \`(b-a)\` that has the **same area** as the region under \`f\`:

\`\`\`
f_avg · (b-a) = ∫_a^b f(x) dx
\`\`\`

### Examples

- \`f(x) = x\` on \`[0, 4]\`: \`f_avg = (1/4)·8 = 2\` (the midpoint — makes sense for a linear function)
- \`f(x) = x²\` on \`[0, 3]\`: \`f_avg = (1/3)·9 = 3\`
- \`f(x) = 1\` on any interval: \`f_avg = 1\` (average of a constant is itself)

### Applications

- Average temperature over a day
- Average power consumed over a cycle
- Average speed over a journey (area under speed-time curve divided by time)

### Your Task

Implement \`double average_value(double (*f)(double), double a, double b, int n)\` using the midpoint rule internally.`,

	starterCode: `#include <stdio.h>

double average_value(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + (i + 0.5) * h);
\t}
\treturn sum * h / (b - a);
}

double id(double x) { return x; }

int main() {
\t/* average of x on [0,4] = 2 */
\tprintf("%.4f\\n", average_value(id, 0.0, 4.0, 10000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double average_value(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + (i + 0.5) * h);
\t}
\treturn sum * h / (b - a);
}

double id(double x) { return x; }

int main() {
\tprintf("%.4f\\n", average_value(id, 0.0, 4.0, 10000));
\treturn 0;
}
`,

	tests: [
		{
			name: "average of x on [0,4] is 2",
			expected: "2.0000\n",
		},
		{
			name: "average of constant 1 on any interval is 1",
			code: `#include <stdio.h>
{{FUNC}}
double one(double x) { return 1.0; }
int main() {
\tprintf("%.4f\\n", average_value(one, 0.0, 5.0, 1000));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "average of x^2 on [0,3] is 3",
			code: `#include <stdio.h>
{{FUNC}}
double quad(double x) { return x * x; }
int main() {
\tprintf("%.4f\\n", average_value(quad, 0.0, 3.0, 10000));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "average of constant 2 on [-1,5] is 2",
			code: `#include <stdio.h>
{{FUNC}}
double two(double x) { return 2.0; }
int main() {
\tprintf("%.4f\\n", average_value(two, -1.0, 5.0, 1000));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
	],
};
