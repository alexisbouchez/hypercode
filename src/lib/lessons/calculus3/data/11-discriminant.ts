import type { Lesson } from "../../types";

export const discriminant2d: Lesson = {
	id: "discriminant_2d",
	title: "Second Derivative Test",
	chapterId: "optimization",
	content: `## Critical Points and the Second Derivative Test

For a function f(x,y), critical points occur where ∇f = 0 (both partial derivatives are zero).

### The Discriminant

At a critical point (x₀, y₀), classify it using:

\`\`\`
D = fxx · fyy - (fxy)²
\`\`\`

Where:
- fxx = ∂²f/∂x²
- fyy = ∂²f/∂y²
- fxy = ∂²f/∂x∂y (mixed partial)

### Classification Rules

| D | fxx | Type |
|---|-----|------|
| D > 0 | fxx > 0 | Local **minimum** |
| D > 0 | fxx < 0 | Local **maximum** |
| D < 0 | — | **Saddle point** |
| D = 0 | — | Test **inconclusive** |

### Examples

**f(x,y) = x² + y²** at (0,0):
- fxx = 2, fyy = 2, fxy = 0
- D = 4 - 0 = 4 > 0, fxx > 0 → **local minimum** ✓

**f(x,y) = x² - y²** at (0,0):
- fxx = 2, fyy = -2, fxy = 0
- D = -4 < 0 → **saddle point** ✓

**f(x,y) = -(x² + y²)** at (0,0):
- fxx = -2, fyy = -2, fxy = 0
- D = 4 > 0, fxx < 0 → **local maximum** ✓

### Computing fxy Numerically

\`\`\`
fxy ≈ [f(x+h,y+h) - f(x+h,y-h) - f(x-h,y+h) + f(x-h,y-h)] / (4h²)
\`\`\`

### Your Task

Implement \`double discriminant_2d(double (*f)(double, double), double x, double y, double h)\` that computes D = fxx·fyy - fxy².`,

	starterCode: `#include <stdio.h>

double discriminant_2d(double (*f)(double, double),
                        double x, double y, double h) {
\t/* D = fxx*fyy - fxy^2 */
\treturn 0.0;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\t/* D for x^2+y^2 at (0,0) = 4 */
\tprintf("%.4f\\n", discriminant_2d(f, 0.0, 0.0, 1e-4));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double discriminant_2d(double (*f)(double, double),
                        double x, double y, double h) {
\tdouble fxx = (f(x + h, y) - 2.0*f(x, y) + f(x - h, y)) / (h * h);
\tdouble fyy = (f(x, y + h) - 2.0*f(x, y) + f(x, y - h)) / (h * h);
\tdouble fxy = (f(x+h,y+h) - f(x+h,y-h) - f(x-h,y+h) + f(x-h,y-h)) / (4.0*h*h);
\treturn fxx*fyy - fxy*fxy;
}

double f(double x, double y) { return x*x + y*y; }

int main() {
\tprintf("%.4f\\n", discriminant_2d(f, 0.0, 0.0, 1e-4));
\treturn 0;
}
`,

	tests: [
		{
			name: "D for x²+y² at (0,0) = 4 (local min)",
			expected: "4.0000\n",
		},
		{
			name: "D for x²-y² at (0,0) = -4 (saddle)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x*x - y*y; }
int main() {
\tprintf("%.4f\\n", discriminant_2d(g, 0.0, 0.0, 1e-4));
\treturn 0;
}`,
			expected: "-4.0000\n",
		},
		{
			name: "D for -(x²+y²) at (0,0) = 4 (local max)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return -(x*x + y*y); }
int main() {
\tprintf("%.4f\\n", discriminant_2d(g, 0.0, 0.0, 1e-4));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "D for x·y at (0,0) = -1 (saddle)",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y) { return x * y; }
int main() {
\tprintf("%.4f\\n", discriminant_2d(g, 0.0, 0.0, 1e-4));
\treturn 0;
}`,
			expected: "-1.0000\n",
		},
	],
};
