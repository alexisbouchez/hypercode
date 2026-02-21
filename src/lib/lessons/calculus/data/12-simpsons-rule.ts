import type { Lesson } from "../../types";

export const simpsonsRule: Lesson = {
	id: "simpsons-rule",
	title: "Simpson's Rule",
	chapterId: "integration",
	content: `## Simpson's Rule

**Simpson's rule** fits a **parabola** through each pair of subintervals (3 points: left, mid, right) instead of a line (trapezoid).

### Formula

With \`n\` subintervals (**n must be even**), \`h = (b-a)/n\`:

\`\`\`
h/3 · [f(x₀) + 4f(x₁) + 2f(x₂) + 4f(x₃) + 2f(x₄) + ... + 4f(xₙ₋₁) + f(xₙ)]
\`\`\`

Pattern of coefficients: **1, 4, 2, 4, 2, ..., 4, 1**

Odd-indexed points get weight **4**, even-indexed interior points get weight **2**, endpoints get weight **1**.

### Why 1-4-2-4-1?

Each pair of subintervals uses the exact integral of the parabola through 3 points, which equals \`h/3·(f(a) + 4f(mid) + f(b))\`. Combining n/2 such pairs gives the pattern.

### Accuracy

Error is \`O(h⁴)\` — much better than trapezoid (\`O(h²)\`):

\`\`\`
Error ≤ (b-a)⁵/(180n⁴) · max|f⁽⁴⁾(x)|
\`\`\`

Simpson's rule is **exact for polynomials of degree ≤ 3**.

### Comparison for \`∫₀¹ x² dx = 1/3\`

| n | Trapezoid | Simpson's |
|---|-----------|-----------|
| 2 | 0.375 | **0.3333** (exact!) |
| 4 | 0.344 | **0.3333** (exact!) |

Simpson's rule gives the exact answer for x² with just n=2!

### Your Task

Implement \`double simpson(double (*f)(double), double a, double b, int n)\`. Assume \`n\` is even.`,

	starterCode: `#include <stdio.h>

double simpson(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = f(a) + f(b);
\tfor (int i = 1; i < n; i++) {
\t\tsum += (i % 2 == 0 ? 2.0 : 4.0) * f(a + i * h);
\t}
\treturn sum * h / 3.0;
}

double quad(double x) { return x * x; }

int main() {
\t/* x^2 from 0 to 1: Simpson is exact -> 1/3 */
\tprintf("%.4f\\n", simpson(quad, 0.0, 1.0, 2));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double simpson(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = f(a) + f(b);
\tfor (int i = 1; i < n; i++) {
\t\tsum += (i % 2 == 0 ? 2.0 : 4.0) * f(a + i * h);
\t}
\treturn sum * h / 3.0;
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", simpson(quad, 0.0, 1.0, 2));
\treturn 0;
}
`,

	tests: [
		{
			name: "x^2 from 0 to 1 with n=2: exactly 1/3",
			expected: "0.3333\n",
		},
		{
			name: "x^3 from 0 to 2 with n=2: exactly 4",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", simpson(cubic, 0.0, 2.0, 2));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "constant 1 from 0 to 3 with n=6 is 3",
			code: `#include <stdio.h>
{{FUNC}}
double one(double x) { return 1.0; }
int main() {
\tprintf("%.4f\\n", simpson(one, 0.0, 3.0, 6));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "x^2 from 0 to 2 with n=4: exactly 8/3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", simpson(quad, 0.0, 2.0, 4));
\treturn 0;
}`,
			expected: "2.6667\n",
		},
	],
};
