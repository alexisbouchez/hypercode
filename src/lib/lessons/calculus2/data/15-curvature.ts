import type { Lesson } from "../../types";

export const curvature: Lesson = {
	id: "curvature",
	title: "Curvature",
	chapterId: "parametric-and-polar",
	content: `## Curvature

**Curvature** $\kappa$ measures how sharply a curve bends at a point. For $y = f(x)$:

$$\kappa = \frac{|f''(x)|}{\left(1 + [f'(x)]^2\right)^{3/2}}$$

### Intuition

- **Straight line**: $f'' = 0$, so $\kappa = 0$ (no bending)
- **Circle of radius $R$**: $\kappa = \frac{1}{R}$ everywhere (constant curvature)
- **Parabola** $y = x^2$ at $x=0$: tightest bend is at the vertex

The **radius of curvature** is $R = \frac{1}{\kappa}$ — the radius of the osculating (best-fit) circle.

### Example: Parabola $y = x^2$

$$f'(x) = 2x, \quad f''(x) = 2$$
$$\kappa(x) = \frac{2}{(1 + 4x^2)^{3/2}}$$

At $x=0$: $\kappa = \frac{2}{1} = 2$ (tightest)

At $x=1$: $\kappa = \frac{2}{(1+4)^{3/2}} = \frac{2}{5\sqrt{5}} \approx 0.1789$ (much gentler)

As $x \to \infty$: $\kappa \to 0$ (nearly straight)

### Computing $(1+f'^2)^{3/2}$

Without \`pow()\`, use the identity $u^{3/2} = u \cdot \sqrt{u}$:
\`\`\`c
double u = 1.0 + fp * fp;
double kappa = fpp_abs / (u * my_sqrt(u));
\`\`\`

### Your Task

Implement \`double curvature(double (*f)(double), double x, double h)\` using central differences for $f'$ and $f''$.`,

	starterCode: `#include <stdio.h>

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double curvature(double (*f)(double), double x, double h) {
\tdouble fp  = (f(x + h) - f(x - h)) / (2.0 * h);
\tdouble fpp = (f(x + h) - 2.0 * f(x) + f(x - h)) / (h * h);
\tdouble fpp_abs = fpp < 0.0 ? -fpp : fpp;
\tdouble u = 1.0 + fp * fp;
\treturn fpp_abs / (u * my_sqrt(u));
}

double parabola(double x) { return x * x; }

int main() {
\t/* y=x^2 at x=0: kappa = 2/(1+0)^(3/2) = 2.0000 */
\tprintf("%.4f\\n", curvature(parabola, 0.0, 1e-4));
\treturn 0;
}`,

	solution: `#include <stdio.h>

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double curvature(double (*f)(double), double x, double h) {
\tdouble fp  = (f(x + h) - f(x - h)) / (2.0 * h);
\tdouble fpp = (f(x + h) - 2.0 * f(x) + f(x - h)) / (h * h);
\tdouble fpp_abs = fpp < 0.0 ? -fpp : fpp;
\tdouble u = 1.0 + fp * fp;
\treturn fpp_abs / (u * my_sqrt(u));
}

double parabola(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", curvature(parabola, 0.0, 1e-4));
\treturn 0;
}`,

	tests: [
		{
			name: "y=x² at x=0: κ = 2.0000 (maximum curvature of parabola)",
			expected: "2.0000\n",
		},
		{
			name: "y=x (line) at x=5: κ = 0.0000 (straight line, no curvature)",
			code: `#include <stdio.h>
{{FUNC}}
double line(double x) { return x; }
int main() {
\tprintf("%.4f\\n", curvature(line, 5.0, 1e-4));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "y=x² at x=1: κ = 2/(5√5) ≈ 0.1789",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", curvature(parabola, 1.0, 1e-4));
\treturn 0;
}`,
			expected: "0.1789\n",
		},
		{
			name: "y=4x² at x=0: κ = 8/(1)^(3/2) = 8.0000",
			code: `#include <stdio.h>
{{FUNC}}
double wide(double x) { return 4.0 * x * x; }
int main() {
\tprintf("%.4f\\n", curvature(wide, 0.0, 1e-4));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
	],
};
