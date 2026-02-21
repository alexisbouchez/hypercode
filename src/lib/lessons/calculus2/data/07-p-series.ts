import type { Lesson } from "../../types";

export const pSeries: Lesson = {
	id: "p-series",
	title: "p-Series",
	chapterId: "sequences-and-series",
	content: `## p-Series

A **p-series** has the form:

$$\sum_{k=1}^{\infty} \frac{1}{k^p} = 1 + \frac{1}{2^p} + \frac{1}{3^p} + \cdots$$

### Convergence Test

The p-series converges if and only if **$p > 1$**.

| $p$ | Series | Converges? | Limit |
|-----|--------|------------|-------|
| $1$ | harmonic $\sum 1/k$ | No | $\infty$ |
| $2$ | Basel $\sum 1/k^2$ | Yes | $\pi^2/6 \approx 1.6449$ |
| $3$ | Apéry $\sum 1/k^3$ | Yes | $\approx 1.2021$ |
| $4$ | $\sum 1/k^4$ | Yes | $\pi^4/90 \approx 1.0823$ |

### Why p=1 Diverges

Group the harmonic series:

$$1 + \frac{1}{2} + \left(\frac{1}{3}+\frac{1}{4}\right) + \left(\frac{1}{5}+\cdots+\frac{1}{8}\right) + \cdots$$

Each group sums to at least $\frac{1}{2}$, so the total diverges.

### Why p=2 Converges

The Basel problem — Euler showed the sum equals $\frac{\pi^2}{6}$ using the factorization of $\frac{\sin(x)}{x}$. Numerically, the partial sums converge slowly: $S_{1000} \approx 1.6439$, approaching $1.6449$.

### Your Task

Implement \`double p_series(int p, int n)\` that computes $\sum_{k=1}^n \frac{1}{k^p}$.

Use a loop to compute $k^p$ without \`math.h\`.`,

	starterCode: `#include <stdio.h>

static double int_pow(double base, int exp) {
\tdouble result = 1.0;
\tfor (int i = 0; i < exp; i++) result *= base;
\treturn result;
}

double p_series(int p, int n) {
\tdouble sum = 0.0;
\tfor (int k = 1; k <= n; k++) {
\t\tsum += 1.0 / int_pow((double)k, p);
\t}
\treturn sum;
}

int main() {
\t/* Σ_{k=1}^4 1/k = 1 + 1/2 + 1/3 + 1/4 ~ 2.0833 */
\tprintf("%.4f\\n", p_series(1, 4));
\treturn 0;
}`,

	solution: `#include <stdio.h>

static double int_pow(double base, int exp) {
\tdouble result = 1.0;
\tfor (int i = 0; i < exp; i++) result *= base;
\treturn result;
}

double p_series(int p, int n) {
\tdouble sum = 0.0;
\tfor (int k = 1; k <= n; k++) {
\t\tsum += 1.0 / int_pow((double)k, p);
\t}
\treturn sum;
}

int main() {
\tprintf("%.4f\\n", p_series(1, 4));
\treturn 0;
}`,

	tests: [
		{
			name: "p=1, n=4: harmonic partial sum ≈ 2.0833",
			expected: "2.0833\n",
		},
		{
			name: "p=2, n=5: 1+1/4+1/9+1/16+1/25 ≈ 1.4636",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", p_series(2, 5));
\treturn 0;
}`,
			expected: "1.4636\n",
		},
		{
			name: "p=3, n=3: 1+1/8+1/27 ≈ 1.1620",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", p_series(3, 3));
\treturn 0;
}`,
			expected: "1.1620\n",
		},
		{
			name: "p=2, n=1: just the first term = 1.0000",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", p_series(2, 1));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
	],
};
