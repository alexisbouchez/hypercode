import type { Lesson } from "../../types";

export const alternatingSeries: Lesson = {
	id: "alternating-series",
	title: "Alternating Series",
	chapterId: "sequences-and-series",
	content: `## Alternating Series

An **alternating series** has terms that switch sign:

\`\`\`
Σ_{k=1}^∞ (-1)^{k+1} a_k = a_1 - a_2 + a_3 - a_4 + ...
\`\`\`

### Alternating Series Test (Leibniz)

If \`a_k > 0\`, \`a_k\` is decreasing, and \`a_k → 0\`, then the series **converges**.

### Famous Example: Alternating Harmonic

\`\`\`
1 - 1/2 + 1/3 - 1/4 + ... = ln(2) ≈ 0.6931
\`\`\`

The partial sums oscillate around \`ln(2)\`, narrowing with each term.

### Error Bound

The error of stopping at term \`n\` is at most \`|a_{n+1}|\` — the next term's absolute value. This makes alternating series easy to approximate with controlled accuracy.

### Comparison

| n | S_n (alternating harmonic) |
|---|---------------------------|
| 1 | 1.0000 |
| 2 | 0.5000 |
| 3 | 0.8333 |
| 4 | 0.5833 |
| 5 | 0.7833 |
| ∞ | 0.6931 |

The partial sums zigzag toward ln(2), each one closer than the last.

### Your Task

Implement \`double alternating_sum(double (*a_n)(int), int n)\` that computes \`Σ_{k=1}^n (-1)^{k+1} a_n(k)\`.`,

	starterCode: `#include <stdio.h>

double alternating_sum(double (*a_n)(int), int n) {
\tdouble sum = 0.0;
\tfor (int k = 1; k <= n; k++) {
\t\tdouble sign = (k % 2 == 1) ? 1.0 : -1.0;
\t\tsum += sign * a_n(k);
\t}
\treturn sum;
}

double inv_k(int k) { return 1.0 / (double)k; }

int main() {
\t/* 1 - 1/2 + 1/3 - 1/4 + 1/5 ~ 0.7833 */
\tprintf("%.4f\\n", alternating_sum(inv_k, 5));
\treturn 0;
}`,

	solution: `#include <stdio.h>

double alternating_sum(double (*a_n)(int), int n) {
\tdouble sum = 0.0;
\tfor (int k = 1; k <= n; k++) {
\t\tdouble sign = (k % 2 == 1) ? 1.0 : -1.0;
\t\tsum += sign * a_n(k);
\t}
\treturn sum;
}

double inv_k(int k) { return 1.0 / (double)k; }

int main() {
\tprintf("%.4f\\n", alternating_sum(inv_k, 5));
\treturn 0;
}`,

	tests: [
		{
			name: "alternating harmonic n=5: 1-1/2+1/3-1/4+1/5 ≈ 0.7833",
			expected: "0.7833\n",
		},
		{
			name: "alternating harmonic n=1: just 1.0000",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", alternating_sum(inv_k, 1));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "alternating 1/k², n=4: 1-1/4+1/9-1/16 ≈ 0.7986",
			code: `#include <stdio.h>
{{FUNC}}
double inv_sq(int k) { return 1.0 / ((double)k * k); }
int main() {
\tprintf("%.4f\\n", alternating_sum(inv_sq, 4));
\treturn 0;
}`,
			expected: "0.7986\n",
		},
		{
			name: "alternating 1/(k(k+1)), n=3: 1/2-1/6+1/12 = 5/12 ≈ 0.4167",
			code: `#include <stdio.h>
{{FUNC}}
double telescope(int k) { return 1.0 / ((double)k * (k + 1)); }
int main() {
\tprintf("%.4f\\n", alternating_sum(telescope, 3));
\treturn 0;
}`,
			expected: "0.4167\n",
		},
	],
};
