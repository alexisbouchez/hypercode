import type { Lesson } from "../../types";

export const meanValueTheorem: Lesson = {
	id: "mean-value-theorem",
	title: "Mean Value Theorem",
	chapterId: "derivative-applications",
	content: `## Mean Value Theorem

The **Mean Value Theorem (MVT)** is one of the most important theorems in calculus:

> If $f$ is continuous on $[a, b]$ and differentiable on $(a, b)$, then there exists at least one point $c \\in (a, b)$ such that:
>
> $$f'(c) = \\frac{f(b) - f(a)}{b - a}$$

In plain English: at some point, the **instantaneous rate of change** equals the **average rate of change** over the interval.

### Geometric Interpretation

Draw the secant line through $(a, f(a))$ and $(b, f(b))$. The MVT says there is at least one point where the tangent line is **parallel** to that secant line.

### Finding the MVT Point Numerically

We want $c$ where $f'(c) = \\frac{f(b) - f(a)}{b - a}$. This is a root-finding problem on $g(x) = f'(x) - \\text{slope}$, solved by bisection.

### Example

For $f(x) = x^2$ on $[0, 4]$:
- Average slope: $\\frac{16 - 0}{4 - 0} = 4$
- $f'(c) = 2c = 4$ → $c = 2$ ✓ (which is the midpoint — always true for parabolas)

### Applications

- **Speed enforcement**: if your average speed between two cameras was 80 mph, at some moment you were going exactly 80 mph
- **Proof of L'Hôpital's rule**
- **Error estimation** in numerical integration
- **Rolle's Theorem**: special case where $f(a) = f(b)$, implying $f'(c) = 0$

### Your Task

Implement \`double mvt_point(double (*f)(double), double a, double b, int n, double h)\` that finds the MVT point $c$ using bisection on $g(x) = f'(x) - \\text{average\\_slope}$.`,

	starterCode: `#include <stdio.h>

double mvt_point(double (*f)(double), double a, double b, int n, double h) {
\tdouble avg_slope = (f(b) - f(a)) / (b - a);
\tdouble lo = a, hi = b;
\tdouble flo = (f(lo + h) - f(lo - h)) / (2.0 * h) - avg_slope;
\tfor (int i = 0; i < n; i++) {
\t\tdouble m = (lo + hi) / 2.0;
\t\tdouble fm = (f(m + h) - f(m - h)) / (2.0 * h) - avg_slope;
\t\tif (flo * fm < 0.0) { hi = m; }
\t\telse { lo = m; flo = fm; }
\t}
\treturn (lo + hi) / 2.0;
}

double quad(double x) { return x * x; }

int main() {
\t/* x^2 on [0,4]: avg slope=4, f'(c)=2c=4 -> c=2 */
\tprintf("%.4f\\n", mvt_point(quad, 0.0, 4.0, 60, 1e-6));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double mvt_point(double (*f)(double), double a, double b, int n, double h) {
\tdouble avg_slope = (f(b) - f(a)) / (b - a);
\tdouble lo = a, hi = b;
\tdouble flo = (f(lo + h) - f(lo - h)) / (2.0 * h) - avg_slope;
\tfor (int i = 0; i < n; i++) {
\t\tdouble m = (lo + hi) / 2.0;
\t\tdouble fm = (f(m + h) - f(m - h)) / (2.0 * h) - avg_slope;
\t\tif (flo * fm < 0.0) { hi = m; }
\t\telse { lo = m; flo = fm; }
\t}
\treturn (lo + hi) / 2.0;
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", mvt_point(quad, 0.0, 4.0, 60, 1e-6));
\treturn 0;
}
`,

	tests: [
		{
			name: "x^2 on [0,4]: MVT point is c=2",
			expected: "2.0000\n",
		},
		{
			name: "x^2 on [1,5]: avg slope=6, c=3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", mvt_point(quad, 1.0, 5.0, 60, 1e-6));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "x^3 on [0,3]: avg slope=9, 3c^2=9 -> c=sqrt(3)~1.7321",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", mvt_point(cubic, 0.0, 3.0, 60, 1e-6));
\treturn 0;
}`,
			expected: "1.7321\n",
		},
		{
			name: "x^3-x on [0,2]: avg slope=3, 3c^2-1=3 -> c=2/sqrt(3)~1.1547",
			code: `#include <stdio.h>
{{FUNC}}
double f(double x) { return x * x * x - x; }
int main() {
\tprintf("%.4f\\n", mvt_point(f, 0.0, 2.0, 60, 1e-6));
\treturn 0;
}`,
			expected: "1.1547\n",
		},
	],
};
