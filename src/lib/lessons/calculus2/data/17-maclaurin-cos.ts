import type { Lesson } from "../../types";

export const maclaurinCos: Lesson = {
	id: "maclaurin-cos",
	title: "Maclaurin Series for cos(x)",
	chapterId: "taylor-series",
	content: `## Maclaurin Series for $\\cos(x)$

The **Maclaurin series** is a Taylor series centred at $x = 0$. For cosine:

$$\\cos(x) = \\sum_{k=0}^{\\infty} \\frac{(-1)^k}{(2k)!} x^{2k} = 1 - \\frac{x^2}{2!} + \\frac{x^4}{4!} - \\frac{x^6}{6!} + \\cdots$$

This series converges for **all real $x$**.

### Building Terms Iteratively

Computing each term from scratch (with a factorial and power) is slow. Instead, use the recurrence:

$$t_{k+1} = t_k \\cdot \\frac{-x^2}{(2k+1)(2k+2)}$$

Starting with $t_0 = 1$:

\`\`\`c
double maclaurin_cos(double x, int terms) {
    double sum = 0.0, term = 1.0;
    for (int k = 0; k < terms; k++) {
        sum += term;
        term *= -x * x / ((2.0*k + 1) * (2.0*k + 2));
    }
    return sum;
}
\`\`\`

### Accuracy vs. Terms

| Terms | $\\cos(\\pi)$ approximation | Error |
|-------|--------------------------|-------|
| 3 | $-1.2337$ | 23% |
| 5 | $-0.9997$ | 0.03% |
| 10 | $-1.0000$ | $\\approx 10^{-9}$ |

### Key Values

\`\`\`
cos(0)     = 1
cos(π/2)   = 0
cos(π)     = -1
cos(2π)    = 1
\`\`\`

### Your Task

Implement \`double maclaurin_cos(double x, int terms)\` using the iterative term recurrence.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double maclaurin_cos(double x, int terms) {
\t/* sum terms of the cosine Maclaurin series */
\treturn 0.0;
}

int main() {
\tprintf("%.4f\\n", maclaurin_cos(0.0, 10));    /* cos(0) = 1 */
\tprintf("%.4f\\n", maclaurin_cos(M_PI, 10));   /* cos(π) = -1 */
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double maclaurin_cos(double x, int terms) {
\tdouble neg_x2 = -(x * x);
\tdouble sum = 0.0, term = 1.0;
\tfor (int k = 0; k < terms; k++) {
\t\tsum += term;
\t\tterm *= neg_x2 / ((2.0 * k + 1.0) * (2.0 * k + 2.0));
\t}
\treturn sum;
}

int main() {
\tprintf("%.4f\\n", maclaurin_cos(0.0, 10));
\tprintf("%.4f\\n", maclaurin_cos(1.0, 10));
\treturn 0;
}
`,

	tests: [
		{
			name: "cos(0) = 1.0000, cos(1) ≈ 0.5403",
			expected: "1.0000\n0.5403\n",
		},
		{
			name: "cos(3) ≈ -0.9900 with 10 terms",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", maclaurin_cos(3.0, 10));
\treturn 0;
}`,
			expected: "-0.9900\n",
		},
		{
			name: "cos(0.5) ≈ 0.8776 with 8 terms",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", maclaurin_cos(0.5, 8));
\treturn 0;
}`,
			expected: "0.8776\n",
		},
		{
			name: "cos(2) ≈ -0.4161 with 12 terms",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", maclaurin_cos(2.0, 12));
\treturn 0;
}`,
			expected: "-0.4161\n",
		},
	],
};
