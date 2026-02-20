import type { Lesson } from "../../types";

export const taylorError: Lesson = {
	id: "taylor-error",
	title: "Taylor Series Error",
	chapterId: "taylor-series",
	content: `## Taylor Series Error

The error of a degree-\`n\` Taylor approximation is:

\`\`\`
E_n(x) = |f(x) - P_n(x)|
\`\`\`

### Taylor's Remainder Theorem

There exists some \`c\` between \`a\` and \`x\` such that:

\`\`\`
E_n(x) = |f^{(n+1)}(c)| / (n+1)! · |x-a|^{n+1}
\`\`\`

This bounds the error by the **next term** of the series (at the worst \`c\`).

### Intuition

- The degree-\`n\` polynomial matches \`f\` through all derivatives up to order \`n\` at \`a\`
- The first "mismatch" comes from the \`(n+1)\`-th derivative
- The further \`x\` is from \`a\`, the larger the error (raised to the \`n+1\` power)

### Example: x³ with P_2

For \`f(x) = x³\`, the Taylor polynomial at \`a=0\` with \`n=2\`:
\`\`\`
P_2(x) = f(0) + f'(0)x + f''(0)/2!·x²
       = 0 + 0 + 0 = 0
\`\`\`
At \`x=2\`: error = |8 - 0| = **8**.

The bound from the theorem: \`|f'''(c)|/3! · |2|³ = 6/6 · 8 = 8\`. Tight!

### Convergence Check

For polynomial \`f(x) = x^k\`, a Taylor polynomial of degree \`k\` at \`a=0\` has **zero error** (it's exact).

### Your Task

Implement \`double taylor_error(double (*f)(double), double a, double x, int n, double h)\` that returns \`|f(x) - P_n(x)|\`.

Helper functions from the Taylor polynomial lesson are provided.`,

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

static double taylor_poly(double (*f)(double), double a, double x, int n, double h) {
\tdouble sum = 0.0;
\tfor (int k = 0; k <= n; k++) {
\t\tsum += nth_deriv(f, a, k, h) / factorial(k) * powi(x - a, k);
\t}
\treturn sum;
}

double taylor_error(double (*f)(double), double a, double x, int n, double h) {
\tdouble diff = f(x) - taylor_poly(f, a, x, n, h);
\treturn diff < 0.0 ? -diff : diff;
}

double cubic(double x) { return x * x * x; }

int main() {
\t/* x^3, P_2 at a=0, x=2: P_2=0, error=|8-0|=8 */
\tprintf("%.4f\\n", taylor_error(cubic, 0.0, 2.0, 2, 1e-3));
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

static double taylor_poly(double (*f)(double), double a, double x, int n, double h) {
\tdouble sum = 0.0;
\tfor (int k = 0; k <= n; k++) {
\t\tsum += nth_deriv(f, a, k, h) / factorial(k) * powi(x - a, k);
\t}
\treturn sum;
}

double taylor_error(double (*f)(double), double a, double x, int n, double h) {
\tdouble diff = f(x) - taylor_poly(f, a, x, n, h);
\treturn diff < 0.0 ? -diff : diff;
}

double cubic(double x) { return x * x * x; }

int main() {
\tprintf("%.4f\\n", taylor_error(cubic, 0.0, 2.0, 2, 1e-3));
\treturn 0;
}`,

	tests: [
		{
			name: "x³, P_2 at a=0, x=2: error = 8.0000",
			expected: "8.0000\n",
		},
		{
			name: "x², P_2 at a=0, x=2: exact polynomial, error ≈ 0.0000",
			code: `#include <stdio.h>
static double factorial(int n) { double r=1.0; for(int i=2;i<=n;i++) r*=i; return r; }
static double powi(double base, int exp) { double r=1.0; for(int i=0;i<exp;i++) r*=base; return r; }
static double nth_deriv(double (*f)(double), double x, int n, double h) {
\tif (n==0) return f(x);
\treturn (nth_deriv(f,x+h,n-1,h)-nth_deriv(f,x-h,n-1,h))/(2.0*h);
}
static double taylor_poly(double (*f)(double), double a, double x, int n, double h) {
\tdouble sum=0.0;
\tfor(int k=0;k<=n;k++) sum+=nth_deriv(f,a,k,h)/factorial(k)*powi(x-a,k);
\treturn sum;
}
{{FUNC}}
double quad(double x) { return x * x; }
int main() {
\tprintf("%.4f\\n", taylor_error(quad, 0.0, 2.0, 2, 1e-3));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "x⁴, P_3 at a=0, x=2: P_3=0, error = 16.0000",
			code: `#include <stdio.h>
static double factorial(int n) { double r=1.0; for(int i=2;i<=n;i++) r*=i; return r; }
static double powi(double base, int exp) { double r=1.0; for(int i=0;i<exp;i++) r*=base; return r; }
static double nth_deriv(double (*f)(double), double x, int n, double h) {
\tif (n==0) return f(x);
\treturn (nth_deriv(f,x+h,n-1,h)-nth_deriv(f,x-h,n-1,h))/(2.0*h);
}
static double taylor_poly(double (*f)(double), double a, double x, int n, double h) {
\tdouble sum=0.0;
\tfor(int k=0;k<=n;k++) sum+=nth_deriv(f,a,k,h)/factorial(k)*powi(x-a,k);
\treturn sum;
}
{{FUNC}}
double quart(double x) { return x * x * x * x; }
int main() {
\tprintf("%.4f\\n", taylor_error(quart, 0.0, 2.0, 3, 1e-3));
\treturn 0;
}`,
			expected: "16.0000\n",
		},
		{
			name: "x², P_1 at a=0, x=3: P_1=0, error = 9.0000",
			code: `#include <stdio.h>
static double factorial(int n) { double r=1.0; for(int i=2;i<=n;i++) r*=i; return r; }
static double powi(double base, int exp) { double r=1.0; for(int i=0;i<exp;i++) r*=base; return r; }
static double nth_deriv(double (*f)(double), double x, int n, double h) {
\tif (n==0) return f(x);
\treturn (nth_deriv(f,x+h,n-1,h)-nth_deriv(f,x-h,n-1,h))/(2.0*h);
}
static double taylor_poly(double (*f)(double), double a, double x, int n, double h) {
\tdouble sum=0.0;
\tfor(int k=0;k<=n;k++) sum+=nth_deriv(f,a,k,h)/factorial(k)*powi(x-a,k);
\treturn sum;
}
{{FUNC}}
double quad(double x) { return x * x; }
int main() {
\tprintf("%.4f\\n", taylor_error(quad, 0.0, 3.0, 1, 1e-3));
\treturn 0;
}`,
			expected: "9.0000\n",
		},
	],
};
