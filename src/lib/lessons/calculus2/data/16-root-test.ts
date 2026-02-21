import type { Lesson } from "../../types";

export const rootTest: Lesson = {
	id: "root-test",
	title: "Root Test",
	chapterId: "sequences-and-series",
	content: `## The Root Test

The **root test** (also called Cauchy's root test) determines whether a series $\\sum a_n$ converges by computing:

$$L = \\lim_{n \\to \\infty} \\sqrt[n]{|a_n|} = \\lim_{n \\to \\infty} |a_n|^{1/n}$$

**Conclusion:**
- $L < 1$ → series **converges absolutely**
- $L > 1$ → series **diverges**
- $L = 1$ → **inconclusive** (try another test)

### Comparison with the Ratio Test

Both tests use the same threshold ($L < 1$ converges, $L > 1$ diverges), but the root test is more powerful — it gives a conclusion in some cases where the ratio test is inconclusive. It is especially useful when $a_n$ is a power or exponential expression raised to the $n$-th power.

### Examples

**$\\sum (1/2)^n$** — geometric series:
$$L = \\lim_{n\\to\\infty} \\left(\\frac{1}{2^n}\\right)^{1/n} = \\frac{1}{2} = 0.5 < 1 \\implies \\text{converges}$$

**$\\sum 2^n$** — divergent geometric:
$$L = \\lim_{n\\to\\infty} (2^n)^{1/n} = 2 > 1 \\implies \\text{diverges}$$

**$\\sum n^{-n}$** — ultra-fast convergence:
$$L = \\lim_{n\\to\\infty} (n^{-n})^{1/n} = \\lim 1/n = 0 < 1 \\implies \\text{converges}$$

### Numerical Approximation

For large $n$, $|a_n|^{1/n}$ is a good approximation of $L$:

\`\`\`c
double root_test_limit(double (*a)(int), int n) {
    return pow(fabs(a(n)), 1.0 / (double)n);
}
\`\`\`

### Your Task

Implement \`double root_test_limit(a, n)\` that computes $|a_n|^{1/n}$ for a given term function $a$ and large $n$.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double root_test_limit(double (*a)(int), int n) {
\treturn 0.0;
}

double geo_half(int n) { return pow(0.5, (double)n); }

int main() {
\t/* (1/2)^n: root = 0.5 → converges */
\tprintf("%.4f\\n", root_test_limit(geo_half, 1000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double root_test_limit(double (*a)(int), int n) {
\treturn pow(fabs(a(n)), 1.0 / (double)n);
}

double geo_half(int n) { return pow(0.5, (double)n); }

int main() {
\tprintf("%.4f\\n", root_test_limit(geo_half, 1000));
\treturn 0;
}
`,

	tests: [
		{
			name: "(1/2)^n: root limit = 0.5",
			expected: "0.5000\n",
		},
		{
			name: "2^n: root limit = 2 → diverges",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double geo_two(int n) { return pow(2.0, (double)n); }
int main() {
\tprintf("%.4f\\n", root_test_limit(geo_two, 1000));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "(1/3)^n: root limit = 0.3333 → converges",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double geo_third(int n) { return pow(1.0/3.0, (double)n); }
int main() {
\tprintf("%.4f\\n", root_test_limit(geo_third, 100));
\treturn 0;
}`,
			expected: "0.3333\n",
		},
		{
			name: "n^(-n): root limit = 1/n, at n=10 gives 0.1",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
double nn(int n) {
\tdouble b = (double)n;
\treturn 1.0 / pow(b, b);
}
int main() {
\t/* (10^{-10})^{1/10} = 10^{-1} = 0.1 */
\tprintf("%.4f\\n", root_test_limit(nn, 10));
\treturn 0;
}`,
			expected: "0.1000\n",
		},
	],
};
