import type { Lesson } from "../../types";

export const chainRule: Lesson = {
	id: "chain-rule",
	title: "Chain Rule",
	chapterId: "differentiation-rules",
	content: `## The Chain Rule

The **chain rule** handles derivatives of composed functions $f(g(x))$:

$$\\frac{d}{dx}\\bigl[f(g(x))\\bigr] = f'(g(x)) \\cdot g'(x)$$

### Examples

| $h(x)$ | $f$, $g$ | $h'(x)$ |
|--------|----------|---------|
| $\\sin(x^2)$ | $f=\\sin$, $g=x^2$ | $\\cos(x^2) \\cdot 2x$ |
| $e^{3x}$ | $f=e^u$, $g=3x$ | $3e^{3x}$ |
| $\\sqrt{x^2+1}$ | $f=\\sqrt{u}$, $g=x^2+1$ | $x/\\sqrt{x^2+1}$ |

### Numerical Chain Derivative

We can approximate the chain derivative numerically using the **central difference** formula applied to the composition:

$$\\frac{d}{dx}\\bigl[f(g(x))\\bigr] \\approx \\frac{f(g(x+h)) - f(g(x-h))}{2h}$$

This is just the standard central difference, but applied to the composed function $f \\circ g$:

\`\`\`c
double chain_deriv(double (*f)(double), double (*g)(double),
                   double x, double h) {
    return (f(g(x + h)) - f(g(x - h))) / (2.0 * h);
}
\`\`\`

### Verification

For $h(x) = \\sin(x^2)$ at $x = 1$:
- Analytic: $h'(1) = \\cos(1^2) \\cdot 2 \\cdot 1 = 2\\cos(1) \\approx 1.0806$
- Numerical with $h = 10^{-6}$: should match to 4+ decimal places

### Your Task

Implement \`double chain_deriv(f, g, x, h)\` using the central difference on the composition $f(g(x))$.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double chain_deriv(double (*f)(double), double (*g)(double),
                   double x, double h) {
\t/* central difference of f(g(x)) */
\treturn 0.0;
}

double sq(double x) { return x * x; }

int main() {
\t/* d/dx[sin(x^2)] at x=1 = 2*cos(1) ≈ 1.0806 */
\tprintf("%.4f\\n", chain_deriv(sin, sq, 1.0, 1e-6));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double chain_deriv(double (*f)(double), double (*g)(double),
                   double x, double h) {
\treturn (f(g(x + h)) - f(g(x - h))) / (2.0 * h);
}

double sq(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", chain_deriv(sin, sq, 1.0, 1e-6));
\treturn 0;
}
`,

	tests: [
		{
			name: "d/dx[sin(x²)] at x=1 = 2cos(1) ≈ 1.0806",
			expected: "1.0806\n",
		},
		{
			name: "d/dx[cos(x²)] at x=0 = 0 (sin(0)·0 = 0)",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double cosw(double x) { return cos(x); }
int main() {
\tprintf("%.4f\\n", chain_deriv(cosw, sq, 0.0, 1e-6));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "d/dx[sqrt(x+1)] at x=3 = 1/(2*sqrt(4)) = 0.25",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double shift(double x) { return x + 1.0; }
int main() {
\tprintf("%.4f\\n", chain_deriv(sqrt, shift, 3.0, 1e-6));
\treturn 0;
}`,
			expected: "0.2500\n",
		},
		{
			name: "d/dx[sin(2x)] at x=0 = 2cos(0) = 2",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double dbl(double x) { return 2.0 * x; }
int main() {
\tprintf("%.4f\\n", chain_deriv(sin, dbl, 0.0, 1e-6));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
	],
};
