import type { Lesson } from "../../types";

export const trapezoidRule: Lesson = {
	id: "trapezoid",
	title: "Trapezoidal Rule",
	chapterId: "integration",
	content: `## Trapezoidal Rule

Instead of rectangles, use **trapezoids** — connect adjacent points with line segments:

\`\`\`
h/2 · [f(a) + 2·f(a+h) + 2·f(a+2h) + ... + 2·f(b-h) + f(b)]
\`\`\`

where \`h = (b-a)/n\`.

### Derivation

Each trapezoid has two parallel sides \`f(xᵢ)\` and \`f(xᵢ₊₁)\` and width \`h\`. Its area is \`h·(f(xᵢ) + f(xᵢ₊₁))/2\`. Sum all n trapezoids — the interior points appear twice:

\`\`\`
Area = h/2 · [f(x₀) + 2f(x₁) + 2f(x₂) + ... + 2f(xₙ₋₁) + f(xₙ)]
\`\`\`

### Accuracy

Error is \`O(h²)\` — halving \`n\` reduces error by factor of 4. The trapezoidal rule is **exact for linear functions**.

### Error Formula

\`\`\`
Error ≤ (b-a)³/(12n²) · max|f''(x)|
\`\`\`

For \`f(x) = sin(x)\`, \`max|f''| = 1\`, so on \`[0, π]\` with n=100: error ≤ π³/120000 ≈ 0.000258.

### Comparison

| Rule | Error order | Exact for |
|------|------------|-----------|
| Left/Right | \`O(h)\` | Constants |
| Trapezoid | \`O(h²)\` | Linear |
| Simpson | \`O(h⁴)\` | Cubic |

### Your Task

Implement \`double trapezoid(double (*f)(double), double a, double b, int n)\`.`,

	starterCode: `#include <stdio.h>

double trapezoid(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = f(a) + f(b);
\tfor (int i = 1; i < n; i++) {
\t\tsum += 2.0 * f(a + i * h);
\t}
\treturn sum * h / 2.0;
}

double id(double x) { return x; }

int main() {
\t/* integral of x from 0 to 4 = 8 (exact for linear f) */
\tprintf("%.4f\\n", trapezoid(id, 0.0, 4.0, 4));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double trapezoid(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = f(a) + f(b);
\tfor (int i = 1; i < n; i++) {
\t\tsum += 2.0 * f(a + i * h);
\t}
\treturn sum * h / 2.0;
}

double id(double x) { return x; }

int main() {
\tprintf("%.4f\\n", trapezoid(id, 0.0, 4.0, 4));
\treturn 0;
}
`,

	tests: [
		{
			name: "integral of x from 0 to 4 is 8 (exact for linear)",
			expected: "8.0000\n",
		},
		{
			name: "integral of constant 1 from 0 to 2 is 2",
			code: `#include <stdio.h>
{{FUNC}}
double one(double x) { return 1.0; }
int main() {
\tprintf("%.4f\\n", trapezoid(one, 0.0, 2.0, 10));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "3x+2 from 1 to 3 is 16 (n=1 still exact for linear)",
			code: `#include <stdio.h>
{{FUNC}}
double lin(double x) { return 3.0 * x + 2.0; }
int main() {
\tprintf("%.4f\\n", trapezoid(lin, 1.0, 3.0, 1));
\treturn 0;
}`,
			expected: "16.0000\n",
		},
		{
			name: "x^2 from 0 to 1 with n=1000 approaches 1/3",
			code: `#include <stdio.h>
{{FUNC}}
double quad(double x) { return x * x; }
int main() {
\tprintf("%.3f\\n", trapezoid(quad, 0.0, 1.0, 1000));
\treturn 0;
}`,
			expected: "0.333\n",
		},
	],
};
