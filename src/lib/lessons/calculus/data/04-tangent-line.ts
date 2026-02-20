import type { Lesson } from "../../types";

export const tangentLine: Lesson = {
	id: "tangent-line",
	title: "Tangent Line",
	chapterId: "limits-and-derivatives",
	content: `## Tangent Line

The **tangent line** at a point \`(x₀, f(x₀))\` is the line that just touches the curve there, with slope equal to \`f'(x₀)\`.

### Equation

\`\`\`
y = f(x₀) + f'(x₀) · (x - x₀)
\`\`\`

This is the **point-slope form** of a line, using the derivative as the slope.

### Linearization

The tangent line is also called the **linear approximation** (or linearization) of \`f\` near \`x₀\`:

\`\`\`
f(x) ≈ f(x₀) + f'(x₀) · (x - x₀)   for x near x₀
\`\`\`

This is the foundation of calculus-based approximation. It says: near any point, a smooth curve looks like a straight line.

### Example

For \`f(x) = x²\` at \`x₀ = 3\`:
- \`f(3) = 9\`
- \`f'(3) = 6\`
- Tangent line: \`y = 9 + 6(x - 3) = 6x - 9\`
- At \`x = 4\`: \`y = 15\` (exact: \`f(4) = 16\`, error = 1 — small for nearby x)

### Normal Line

The **normal line** is perpendicular to the tangent:

\`\`\`
y = f(x₀) - (1/f'(x₀)) · (x - x₀)
\`\`\`

### Your Task

Implement \`double tangent_y(double (*f)(double), double x0, double x, double h)\` that returns the y-value of the tangent line at \`x₀\`, evaluated at \`x\`. Use the central difference formula to compute \`f'(x₀)\`.`,

	starterCode: `#include <stdio.h>

double tangent_y(double (*f)(double), double x0, double x, double h) {
\tdouble slope = (f(x0 + h) - f(x0 - h)) / (2.0 * h);
\treturn f(x0) + slope * (x - x0);
}

double quad(double x) { return x * x; }

int main() {
\t/* tangent to x^2 at x0=3, eval at x=4: 9 + 6*(4-3) = 15 */
\tprintf("%.4f\\n", tangent_y(quad, 3.0, 4.0, 1e-6));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double tangent_y(double (*f)(double), double x0, double x, double h) {
\tdouble slope = (f(x0 + h) - f(x0 - h)) / (2.0 * h);
\treturn f(x0) + slope * (x - x0);
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", tangent_y(quad, 3.0, 4.0, 1e-6));
\treturn 0;
}
`,

	tests: [
		{
			name: "tangent to x^2 at x0=3, eval at x=4 is 15",
			expected: "15.0000\n",
		},
		{
			name: "tangent at tangent point equals f(x0)",
			code: `#include <stdio.h>
{{FUNC}}
double quad(double x) { return x * x; }
int main() {
\tprintf("%.4f\\n", tangent_y(quad, 3.0, 3.0, 1e-6));
\treturn 0;
}`,
			expected: "9.0000\n",
		},
		{
			name: "tangent to x^3 at x0=2, eval at x=3: f(2)=8, f'(2)=12, y=20",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", tangent_y(cubic, 2.0, 3.0, 1e-6));
\treturn 0;
}`,
			expected: "20.0000\n",
		},
		{
			name: "tangent to 2x+1 at any x0 is the line itself: at x0=0 eval at x=5 gives 11",
			code: `#include <stdio.h>
{{FUNC}}
double lin(double x) { return 2.0 * x + 1.0; }
int main() {
\tprintf("%.4f\\n", tangent_y(lin, 0.0, 5.0, 1e-6));
\treturn 0;
}`,
			expected: "11.0000\n",
		},
	],
};
