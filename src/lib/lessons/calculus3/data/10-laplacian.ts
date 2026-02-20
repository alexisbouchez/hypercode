import type { Lesson } from "../../types";

export const laplacian2d: Lesson = {
	id: "laplacian_2d",
	title: "Laplacian",
	chapterId: "optimization",
	content: `## The Laplacian

The **Laplacian** of f(x, y) is the sum of its second-order partial derivatives:

\`\`\`
∇²f = ∂²f/∂x² + ∂²f/∂y²
\`\`\`

It measures **how much f differs from its average** in the neighborhood of a point.

### Computing Second Derivatives Numerically

Using central differences twice:

\`\`\`
∂²f/∂x² ≈ [f(x+h,y) - 2f(x,y) + f(x-h,y)] / h²
∂²f/∂y² ≈ [f(x,y+h) - 2f(x,y) + f(x,y-h)] / h²
\`\`\`

### Applications

The Laplacian appears throughout physics and engineering:
- **Heat equation**: ∂T/∂t = α∇²T (how temperature diffuses)
- **Wave equation**: ∂²u/∂t² = c²∇²u
- **Electrostatics**: ∇²φ = 0 in free space (Laplace's equation)
- **Image processing**: edge detection (Laplacian filter)

### Interpretation

- ∇²f > 0 at a point: f is below its local average (like a bowl)
- ∇²f < 0 at a point: f is above its local average (like a hill)
- ∇²f = 0: **harmonic function** — a steady-state distribution

### Example

For f(x,y) = x² + y²:
- ∂²f/∂x² = 2
- ∂²f/∂y² = 2
- ∇²f = 4 everywhere

For f(x,y) = x² - y²:
- ∇²f = 2 + (-2) = 0 (harmonic!)

### Your Task

Implement \`double laplacian_2d(double (*f)(double, double), double x, double y, double h)\` using the central difference formulas for second derivatives.`,

	starterCode: `#include <stdio.h>

double laplacian_2d(double (*f)(double, double),
                    double x, double y, double h) {
\t/* fxx + fyy via central differences */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* ∇²(x²+y²) = 4 everywhere */
\tprintf("%.4f\\n", laplacian_2d(f, 3.0, 4.0, 1e-4));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double laplacian_2d(double (*f)(double, double),
                    double x, double y, double h) {
\tdouble fxx = (f(x + h, y) - 2.0*f(x, y) + f(x - h, y)) / (h * h);
\tdouble fyy = (f(x, y + h) - 2.0*f(x, y) + f(x, y - h)) / (h * h);
\treturn fxx + fyy;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", laplacian_2d(f, 3.0, 4.0, 1e-4));
\treturn 0;
}
`,

	tests: [
		{
			name: "∇²(x²+y²) = 4 everywhere",
			expected: "4.0000\n",
		},
		{
			name: "∇²(x²-y²) = 0 (harmonic)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*x - y*y; }
int main() {
\tprintf("%.4f\\n", laplacian_2d(g, 3.0, 4.0, 1e-4));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "∇²(x³+y³) at (1,1) = 6",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*x*x + y*y*y; }
int main() {
\tprintf("%.4f\\n", laplacian_2d(g, 1.0, 1.0, 1e-4));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
		{
			name: "∇²(x·y) = 0 (harmonic — no pure second partials)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", laplacian_2d(g, 5.0, 7.0, 1e-4));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
