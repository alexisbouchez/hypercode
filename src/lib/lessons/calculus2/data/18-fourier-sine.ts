import type { Lesson } from "../../types";

export const fourierSine: Lesson = {
	id: "fourier-sine",
	title: "Fourier Sine Coefficients",
	chapterId: "fourier-series",
	content: `## Fourier Series

Any periodic function can be expressed as a sum of sines and cosines — the **Fourier series**:

$$f(x) = \\frac{a_0}{2} + \\sum_{n=1}^{\\infty} \\left[a_n \\cos(nx) + b_n \\sin(nx)\\right]$$

for functions on $[-\\pi, \\pi]$. The coefficients are:

$$a_n = \\frac{1}{\\pi}\\int_{-\\pi}^{\\pi} f(x)\\cos(nx)\\,dx \\qquad b_n = \\frac{1}{\\pi}\\int_{-\\pi}^{\\pi} f(x)\\sin(nx)\\,dx$$

### Sine Coefficients of $f(x) = x$

Since $f(x) = x$ is an **odd function**, all cosine coefficients $a_n = 0$. The sine coefficients are:

$$b_n = \\frac{2(-1)^{n+1}}{n}$$

So:
$$x = 2\\sin(x) - \\sin(2x) + \\frac{2}{3}\\sin(3x) - \\cdots \\quad \\text{on } (-\\pi, \\pi)$$

| $n$ | $b_n$ |
|-----|-------|
| 1 | 2.0000 |
| 2 | −1.0000 |
| 3 | 0.6667 |

### Numerical Computation

Approximate the integral using the left Riemann sum with $N$ steps on $[-\\pi, \\pi]$:

$$b_n \\approx \\frac{1}{\\pi} \\sum_{i=0}^{N-1} f(x_i) \\sin(n x_i) \\cdot h \\quad \\text{where } h = \\frac{2\\pi}{N}$$

### Your Task

Implement \`double fourier_bn(f, n, steps)\` that numerically computes the $n$-th Fourier sine coefficient of $f$ on $[-\\pi, \\pi]$.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double fourier_bn(double (*f)(double), int n, int steps) {
\tdouble h = 2.0 * M_PI / steps;
\tdouble sum = 0.0;
\t/* sum f(x_i) * sin(n * x_i) * h, divide by pi */
\treturn 0.0;
}

double identity(double x) { return x; }

int main() {
\t/* b_1 of f(x)=x on [-π,π] = 2 */
\tprintf("%.4f\\n", fourier_bn(identity, 1, 100000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double fourier_bn(double (*f)(double), int n, int steps) {
\tdouble h = 2.0 * M_PI / steps;
\tdouble sum = 0.0;
\tfor (int i = 0; i < steps; i++) {
\t\tdouble x = -M_PI + i * h;
\t\tsum += f(x) * sin((double)n * x);
\t}
\treturn sum * h / M_PI;
}

double identity(double x) { return x; }

int main() {
\tprintf("%.4f\\n", fourier_bn(identity, 1, 100000));
\treturn 0;
}
`,

	tests: [
		{
			name: "b_1 of f(x)=x on [-π,π] = 2",
			expected: "2.0000\n",
		},
		{
			name: "b_2 of f(x)=x = -1",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double linf(double x) { return x; }
int main() {
\tprintf("%.4f\\n", fourier_bn(linf, 2, 10000));
\treturn 0;
}`,
			expected: "-1.0000\n",
		},
		{
			name: "b_1 of sin(x) on [-π,π] = 1 (since sin is its own Fourier term)",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", fourier_bn(sin, 1, 10000));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "b_1 of constant 1 = 0 (even function, no sine terms)",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double one(double x) { return 1.0 + x * 0.0; }
int main() {
\tprintf("%.4f\\n", fourier_bn(one, 1, 10000));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
