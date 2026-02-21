import type { Lesson } from "../../types";

export const secondDerivative: Lesson = {
	id: "second-derivative",
	title: "Second Derivative",
	chapterId: "limits-and-derivatives",
	content: `## The Second Derivative

The **second derivative** $f''(x)$ is the derivative of $f'(x)$ — the rate of change of the slope.

### Geometric Meaning

- $f''(x) > 0$: curve is **concave up** (holds water, like a bowl)
- $f''(x) < 0$: curve is **concave down** (spills water, like a hill)
- $f''(x) = 0$: possible **inflection point** (concavity changes)

### Numerical Formula

The second derivative can be approximated directly from $f$ without computing $f'$ first:

$$f''(x) \\approx \\frac{f(x+h) - 2f(x) + f(x-h)}{h^2}$$

**Derivation**: expand $f(x+h)$ and $f(x-h)$ using Taylor series:

$$f(x+h) = f(x) + h f'(x) + \\frac{h^2}{2} f''(x) + O(h^3)$$

$$f(x-h) = f(x) - h f'(x) + \\frac{h^2}{2} f''(x) + O(h^3)$$

Add them: $f(x+h) + f(x-h) = 2f(x) + h^2 f''(x)$, so:

$$f''(x) = \\frac{f(x+h) - 2f(x) + f(x-h)}{h^2}$$

### Inflection Points

At an inflection point, $f''(c) = 0$ and the sign of $f''$ changes. Example: $f(x) = x^3$ has an inflection at $x = 0$.

### Second Derivative Test

For a critical point $c$ where $f'(c) = 0$:
- $f''(c) > 0$ → local **minimum**
- $f''(c) < 0$ → local **maximum**
- $f''(c) = 0$ → inconclusive

### Your Task

Implement \`double second_derivative(double (*f)(double), double x, double h)\`.`,

	starterCode: `#include <stdio.h>

double second_derivative(double (*f)(double), double x, double h) {
\treturn (f(x + h) - 2.0 * f(x) + f(x - h)) / (h * h);
}

double quad(double x) { return x * x; }

int main() {
\t/* f''(x) = 2 for x^2 */
\tprintf("%.4f\\n", second_derivative(quad, 5.0, 1e-4));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double second_derivative(double (*f)(double), double x, double h) {
\treturn (f(x + h) - 2.0 * f(x) + f(x - h)) / (h * h);
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", second_derivative(quad, 5.0, 1e-4));
\treturn 0;
}
`,

	tests: [
		{
			name: "f''(x) = 2 for x^2",
			expected: "2.0000\n",
		},
		{
			name: "f''(x) = 6x for x^3, at x=5 gives 30",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", second_derivative(cubic, 5.0, 1e-4));
\treturn 0;
}`,
			expected: "30.0000\n",
		},
		{
			name: "f''(x) = 12x^2 for x^4, at x=2 gives 48",
			code: `#include <stdio.h>
{{FUNC}}
double quartic(double x) { return x * x * x * x; }
int main() {
\tprintf("%.4f\\n", second_derivative(quartic, 2.0, 1e-4));
\treturn 0;
}`,
			expected: "48.0000\n",
		},
		{
			name: "f'' of a linear function is 0",
			code: `#include <stdio.h>
{{FUNC}}
double lin(double x) { return 3.0 * x + 7.0; }
int main() {
\tprintf("%.4f\\n", second_derivative(lin, 10.0, 1e-4));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
