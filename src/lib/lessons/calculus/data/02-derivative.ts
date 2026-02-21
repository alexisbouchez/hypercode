import type { Lesson } from "../../types";

export const derivative: Lesson = {
	id: "derivative",
	title: "The Derivative",
	chapterId: "limits-and-derivatives",
	content: `## The Derivative

The **derivative** of \`f\` at \`x\` is the instantaneous rate of change — the slope of the tangent line at that point:

\`\`\`
f'(x) = lim_{h→0} (f(x+h) - f(x)) / h
\`\`\`

### Central Difference Formula

The forward difference \`(f(x+h) - f(x)) / h\` has error \`O(h)\`.
The **central difference** is more accurate — error \`O(h²)\`:

\`\`\`
f'(x) ≈ (f(x+h) - f(x-h)) / (2h)
\`\`\`

This is what you should implement. With \`h = 1e-6\`, results match analytic derivatives to 9+ significant figures.

### Analytic Derivatives

For reference, the exact rules you can verify numerically:

| Function | Derivative |
|----------|-----------|
| \`xⁿ\` | \`n·xⁿ⁻¹\` (power rule) |
| \`sin(x)\` | \`cos(x)\` |
| \`eˣ\` | \`eˣ\` |
| \`ln(x)\` | \`1/x\` |
| \`f(g(x))\` | \`f'(g(x))·g'(x)\` (chain rule) |

### Verification

For \`f(x) = x²\`:
- Analytic: \`f'(2) = 2·2 = 4\`
- Central difference: \`((2+h)² - (2-h)²) / (2h) = 8h / 2h = 4\` ✓ (exact, regardless of h)

### Your Task

Implement \`double derivative(double (*f)(double), double x, double h)\` using the central difference formula.`,

	starterCode: `#include <stdio.h>

double derivative(double (*f)(double), double x, double h) {
\treturn (f(x + h) - f(x - h)) / (2.0 * h);
}

double quad(double x) { return x * x; }

int main() {
\t/* f'(x) = 2x, so f'(3) = 6 */
\tprintf("%.4f\\n", derivative(quad, 3.0, 1e-6));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double derivative(double (*f)(double), double x, double h) {
\treturn (f(x + h) - f(x - h)) / (2.0 * h);
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", derivative(quad, 3.0, 1e-6));
\treturn 0;
}
`,

	tests: [
		{
			name: "d/dx[x^2] at x=3 is 6",
			expected: "6.0000\n",
		},
		{
			name: "d/dx[x^2] at x=2 is 4",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", derivative(quad, 2.0, 1e-6));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "d/dx[x^3] at x=3 is 27",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", derivative(cubic, 3.0, 1e-6));
\treturn 0;
}`,
			expected: "27.0000\n",
		},
		{
			name: "d/dx[5x+2] is 5 everywhere",
			code: `#include <stdio.h>
{{FUNC}}
double lin(double x) { return 5.0 * x + 2.0; }
int main() {
\tprintf("%.4f\\n", derivative(lin, 100.0, 1e-6));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
	],
};
