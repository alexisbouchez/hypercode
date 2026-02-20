import type { Lesson } from "../../types";

export const maclaurinExp: Lesson = {
	id: "maclaurin-exp",
	title: "Maclaurin Series for eˣ",
	chapterId: "taylor-series",
	content: `## Maclaurin Series for eˣ

The most important Taylor series is for \`eˣ\` centered at \`a = 0\` (Maclaurin):

\`\`\`
eˣ = Σ_{k=0}^∞ xᵏ/k! = 1 + x + x²/2! + x³/3! + x⁴/4! + ...
\`\`\`

### Why This Works

Every derivative of \`eˣ\` is \`eˣ\`, so \`f^(k)(0) = e⁰ = 1\` for all \`k\`. The Taylor series becomes:
\`\`\`
P_n(x) = Σ_{k=0}^n xᵏ/k!
\`\`\`

### Convergence

This series converges for **all** \`x\`. By the ratio test:
\`\`\`
|a_{k+1}/a_k| = |x|/(k+1) → 0  as k → ∞
\`\`\`

### How Fast It Converges

For \`x=1\` (computing \`e\`):

| n | P_n(1) | Error |
|---|--------|-------|
| 5 | 2.7167 | 0.0016 |
| 10 | 2.71828 | < 1e-7 |
| 20 | 2.71828182... | machine precision |

### Efficient Evaluation (Horner's Method Alternative)

Instead of computing \`xᵏ/k!\` from scratch each time, update iteratively:
\`\`\`c
double term = 1.0;   // x^0 / 0! = 1
for k = 0 to n:
    sum += term
    term = term * x / (k+1)   // term becomes x^{k+1} / (k+1)!
\`\`\`

Each iteration does just 2 operations — no power or factorial needed.

### Your Task

Implement \`double maclaurin_exp(double x, int n)\` that computes \`Σ_{k=0}^n xᵏ/k!\`.`,

	starterCode: `#include <stdio.h>

double maclaurin_exp(double x, int n) {
\tdouble sum = 0.0;
\tdouble term = 1.0;
\tfor (int k = 0; k <= n; k++) {
\t\tsum += term;
\t\tterm = term * x / (k + 1);
\t}
\treturn sum;
}

int main() {
\t/* e^1 = e ~ 2.7183 */
\tprintf("%.4f\\n", maclaurin_exp(1.0, 20));
\treturn 0;
}`,

	solution: `#include <stdio.h>

double maclaurin_exp(double x, int n) {
\tdouble sum = 0.0;
\tdouble term = 1.0;
\tfor (int k = 0; k <= n; k++) {
\t\tsum += term;
\t\tterm = term * x / (k + 1);
\t}
\treturn sum;
}

int main() {
\tprintf("%.4f\\n", maclaurin_exp(1.0, 20));
\treturn 0;
}`,

	tests: [
		{
			name: "e^1 with n=20 ≈ 2.7183",
			expected: "2.7183\n",
		},
		{
			name: "e^0 = 1.0000 (any n)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", maclaurin_exp(0.0, 10));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "e^2 with n=30 ≈ 7.3891",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", maclaurin_exp(2.0, 30));
\treturn 0;
}`,
			expected: "7.3891\n",
		},
		{
			name: "e^(-1) with n=20 ≈ 0.3679 (1/e)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", maclaurin_exp(-1.0, 20));
\treturn 0;
}`,
			expected: "0.3679\n",
		},
	],
};
