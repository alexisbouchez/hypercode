import type { Lesson } from "../../types";

export const arcLength: Lesson = {
	id: "arc-length",
	title: "Arc Length",
	chapterId: "integration-applications",
	content: `## Arc Length

The **length** of a curve \`y = f(x)\` from \`a\` to \`b\`:

\`\`\`
L = ∫_a^b √(1 + [f'(x)]²) dx
\`\`\`

### Why It Works

At each \`x\`, the curve moves right by \`dx\` and up by \`f'(x)dx\`. By the Pythagorean theorem, the infinitesimal piece has length \`√(dx² + (f'dx)²) = √(1+f'²) dx\`.

### Examples

**Straight line** \`y = mx\` on \`[a, b]\`: \`f' = m\`, so \`L = (b-a)√(1+m²)\`

- \`y = x\` on \`[0,3]\`: \`L = 3√2 ≈ 4.2426\`
- \`y = 3x\` on \`[0,4]\`: \`L = 4√10 ≈ 12.6491\`

### Numerical Approach

Use the midpoint rule with a central difference derivative:

\`\`\`c
double dx = (b - a) / n;
for each midpoint x:
    double deriv = (f(x+h) - f(x-h)) / (2*h);
    sum += my_sqrt(1.0 + deriv * deriv);
return sum * dx;
\`\`\`

### Your Task

Implement \`double arc_length(double (*f)(double), double a, double b, int n, double h)\`.

Use the midpoint rule with step \`dx = (b-a)/n\` and central difference with step \`h\` for the derivative. A Newton's-method sqrt helper is provided.`,

	starterCode: `#include <stdio.h>

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double arc_length(double (*f)(double), double a, double b, int n, double h) {
\tdouble dx = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * dx;
\t\tdouble deriv = (f(x + h) - f(x - h)) / (2.0 * h);
\t\tsum += my_sqrt(1.0 + deriv * deriv);
\t}
\treturn sum * dx;
}

double line(double x) { return x; }

int main() {
\t/* arc length of y=x on [0,3] = 3*sqrt(2) ~ 4.2426 */
\tprintf("%.4f\\n", arc_length(line, 0.0, 3.0, 10000, 1e-5));
\treturn 0;
}`,

	solution: `#include <stdio.h>

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double arc_length(double (*f)(double), double a, double b, int n, double h) {
\tdouble dx = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * dx;
\t\tdouble deriv = (f(x + h) - f(x - h)) / (2.0 * h);
\t\tsum += my_sqrt(1.0 + deriv * deriv);
\t}
\treturn sum * dx;
}

double line(double x) { return x; }

int main() {
\tprintf("%.4f\\n", arc_length(line, 0.0, 3.0, 10000, 1e-5));
\treturn 0;
}`,

	tests: [
		{
			name: "arc length of y=x on [0,3] = 3√2 ≈ 4.2426",
			expected: "4.2426\n",
		},
		{
			name: "arc length of horizontal line y=5 on [0,4] = 4",
			code: `#include <stdio.h>
static double my_sqrt(double x) { double r=x; for(int i=0;i<50;i++) r=(r+x/r)/2.0; return r; }
{{FUNC}}
double horiz(double x) { return 5.0; }
int main() {
\tprintf("%.4f\\n", arc_length(horiz, 0.0, 4.0, 1000, 1e-5));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "arc length of y=3x on [0,4] = 4√10 ≈ 12.6491",
			code: `#include <stdio.h>
static double my_sqrt(double x) { double r=x; for(int i=0;i<50;i++) r=(r+x/r)/2.0; return r; }
{{FUNC}}
double triple(double x) { return 3.0 * x; }
int main() {
\tprintf("%.4f\\n", arc_length(triple, 0.0, 4.0, 10000, 1e-5));
\treturn 0;
}`,
			expected: "12.6491\n",
		},
		{
			name: "arc length of y=2x on [0,1] = √5 ≈ 2.2361",
			code: `#include <stdio.h>
static double my_sqrt(double x) { double r=x; for(int i=0;i<50;i++) r=(r+x/r)/2.0; return r; }
{{FUNC}}
double two_x(double x) { return 2.0 * x; }
int main() {
\tprintf("%.4f\\n", arc_length(two_x, 0.0, 1.0, 10000, 1e-5));
\treturn 0;
}`,
			expected: "2.2361\n",
		},
	],
};
