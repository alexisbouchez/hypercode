import type { Lesson } from "../../types";

export const partialSums: Lesson = {
	id: "partial-sums",
	title: "Partial Sums of Series",
	chapterId: "sequences-and-series",
	content: `## Partial Sums of Series

An infinite series $\sum a_n$ is defined as the limit of **partial sums**:

$$S_n = a_1 + a_2 + \cdots + a_n = \sum_{k=1}^n a_k$$

If $\lim_{n \to \infty} S_n = L$, the series **converges** to $L$.

### Famous Series

**Harmonic series** (diverges):

$$\sum \frac{1}{k} = 1 + \frac{1}{2} + \frac{1}{3} + \frac{1}{4} + \cdots \to \infty$$

**Basel problem** (converges to $\pi^2/6 \approx 1.6449$):

$$\sum \frac{1}{k^2} = 1 + \frac{1}{4} + \frac{1}{9} + \frac{1}{16} + \cdots$$

**Telescoping** (exact finite sum):

$$\sum_{k=1}^n \frac{1}{k(k+1)} = 1 - \frac{1}{n+1}$$

Because $\frac{1}{k(k+1)} = \frac{1}{k} - \frac{1}{k+1}$, most terms cancel.

### Convergence Test: Divergence

If $\lim_{n \to \infty} a_n \neq 0$, the series **must** diverge. (But $a_n \to 0$ alone doesn't guarantee convergence — the harmonic series is the classic counterexample.)

### Your Task

Implement \`double partial_sum(double (*a_n)(int), int n)\` that computes $\sum_{k=1}^n a_n(k)$.`,

	starterCode: `#include <stdio.h>

double partial_sum(double (*a_n)(int), int n) {
\tdouble sum = 0.0;
\tfor (int k = 1; k <= n; k++) {
\t\tsum += a_n(k);
\t}
\treturn sum;
}

double ones(int k) { return 1.0; }

int main() {
\t/* Σ_{k=1}^5 1 = 5 */
\tprintf("%.4f\\n", partial_sum(ones, 5));
\treturn 0;
}`,

	solution: `#include <stdio.h>

double partial_sum(double (*a_n)(int), int n) {
\tdouble sum = 0.0;
\tfor (int k = 1; k <= n; k++) {
\t\tsum += a_n(k);
\t}
\treturn sum;
}

double ones(int k) { return 1.0; }

int main() {
\tprintf("%.4f\\n", partial_sum(ones, 5));
\treturn 0;
}`,

	tests: [
		{
			name: "Σ_{k=1}^5 1 = 5",
			expected: "5.0000\n",
		},
		{
			name: "Σ_{k=1}^4 k = 10",
			code: `#include <stdio.h>
{{FUNC}}
double id(int k) { return (double)k; }
int main() {
\tprintf("%.4f\\n", partial_sum(id, 4));
\treturn 0;
}`,
			expected: "10.0000\n",
		},
		{
			name: "telescoping Σ_{k=1}^5 1/(k(k+1)) = 5/6 ≈ 0.8333",
			code: `#include <stdio.h>
{{FUNC}}
double telescope(int k) { return 1.0 / ((double)k * (k + 1)); }
int main() {
\tprintf("%.4f\\n", partial_sum(telescope, 5));
\treturn 0;
}`,
			expected: "0.8333\n",
		},
		{
			name: "Σ_{k=1}^1000 1/k² ≈ 1.6439 (approaching π²/6)",
			code: `#include <stdio.h>
{{FUNC}}
double inv_sq(int k) { return 1.0 / ((double)k * k); }
int main() {
\tprintf("%.4f\\n", partial_sum(inv_sq, 1000));
\treturn 0;
}`,
			expected: "1.6439\n",
		},
	],
};
