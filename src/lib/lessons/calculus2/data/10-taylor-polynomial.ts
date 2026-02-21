import type { Lesson } from "../../types";

export const taylorPolynomial: Lesson = {
	id: "taylor-polynomial",
	title: "Taylor Polynomial",
	chapterId: "taylor-series",
	content: `## Taylor Polynomial

The **Taylor polynomial of degree n** approximates \`f(x)\` near point \`a\`:

\`\`\`
P_n(x) = Σ_{k=0}^n f^(k)(a)/k! · (x-a)^k
\`\`\`

Where \`f^(k)(a)\` is the k-th derivative of \`f\` at \`a\`.

### Building Intuition

- \`P_0(x) = f(a)\` — constant (matches value at \`a\`)
- \`P_1(x) = f(a) + f'(a)(x-a)\` — linear (also matches slope)
- \`P_2(x)\` — also matches curvature
- Each higher term matches one more derivative at \`a\`

### Exact for Polynomials

A degree-\`n\` Taylor polynomial reproduces any degree-\`n\` polynomial **exactly**:

\`f(x) = x²\` at \`a=0\`:
\`\`\`
P_2(x) = f(0) + f'(0)x + f''(0)/2! x² = 0 + 0 + 2/2·x² = x²  ✓
\`\`\`

### Numerical nth Derivative

Use recursive central differences:
\`\`\`c
f^(0)(x) = f(x)
f^(n)(x) ≈ (f^(n-1)(x+h) - f^(n-1)(x-h)) / (2h)
\`\`\`

This is accurate to \`O(h²)\` at each level. Keep \`n ≤ 4\` and \`h ≈ 1e-3\` for best results.

### Your Task

Implement \`double taylor_poly(double (*f)(double), double a, double x, int n, double h)\`.

Use recursive \`nth_deriv\` and a loop-based \`factorial\`. Helper stubs are provided.`,

	starterCode: `#include <stdio.h>

static double factorial(int n) {
\tdouble r = 1.0;
\tfor (int i = 2; i <= n; i++) r *= i;
\treturn r;
}

static double powi(double base, int exp) {
\tdouble r = 1.0;
\tfor (int i = 0; i < exp; i++) r *= base;
\treturn r;
}

static double nth_deriv(double (*f)(double), double x, int n, double h) {
\tif (n == 0) return f(x);
\treturn (nth_deriv(f, x + h, n - 1, h) - nth_deriv(f, x - h, n - 1, h)) / (2.0 * h);
}

double taylor_poly(double (*f)(double), double a, double x, int n, double h) {
\tdouble sum = 0.0;
\tfor (int k = 0; k <= n; k++) {
\t\tsum += nth_deriv(f, a, k, h) / factorial(k) * powi(x - a, k);
\t}
\treturn sum;
}

double quad(double x) { return x * x; }

int main() {
\t/* P_2 of x^2 at a=0, x=2: = 4 (exact) */
\tprintf("%.4f\\n", taylor_poly(quad, 0.0, 2.0, 2, 1e-3));
\treturn 0;
}`,

	solution: `#include <stdio.h>

static double factorial(int n) {
\tdouble r = 1.0;
\tfor (int i = 2; i <= n; i++) r *= i;
\treturn r;
}

static double powi(double base, int exp) {
\tdouble r = 1.0;
\tfor (int i = 0; i < exp; i++) r *= base;
\treturn r;
}

static double nth_deriv(double (*f)(double), double x, int n, double h) {
\tif (n == 0) return f(x);
\treturn (nth_deriv(f, x + h, n - 1, h) - nth_deriv(f, x - h, n - 1, h)) / (2.0 * h);
}

double taylor_poly(double (*f)(double), double a, double x, int n, double h) {
\tdouble sum = 0.0;
\tfor (int k = 0; k <= n; k++) {
\t\tsum += nth_deriv(f, a, k, h) / factorial(k) * powi(x - a, k);
\t}
\treturn sum;
}

double quad(double x) { return x * x; }

int main() {
\tprintf("%.4f\\n", taylor_poly(quad, 0.0, 2.0, 2, 1e-3));
\treturn 0;
}`,

	tests: [
		{
			name: "P_2 of x² at a=0, x=2: exact = 4.0000",
			expected: "4.0000\n",
		},
		{
			name: "P_3 of x³ at a=0, x=2: exact = 8.0000",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return x * x * x; }
int main() {
\tprintf("%.4f\\n", taylor_poly(cubic, 0.0, 2.0, 3, 1e-3));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
		{
			name: "P_2 of x² at a=1, x=3: exact = 9.0000",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", taylor_poly(quad, 1.0, 3.0, 2, 1e-3));
\treturn 0;
}`,
			expected: "9.0000\n",
		},
		{
			name: "P_1 of x² at a=0, x=3: P_1=0 (misses x² term), = 0.0000",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", taylor_poly(quad, 0.0, 3.0, 1, 1e-3));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
