import type { Lesson } from "../../types";

export const improperIntegrals: Lesson = {
	id: "improper-integrals",
	title: "Improper Integrals",
	chapterId: "integration-applications",
	content: `## Improper Integrals

An integral with an **infinite limit** is defined as a limit:

\`\`\`
∫_a^∞ f(x) dx = lim_{T→∞} ∫_a^T f(x) dx
\`\`\`

If the limit exists, the integral **converges**. Otherwise, it **diverges**.

### Key Examples

| Integral | Result | Converges? |
|----------|--------|------------|
| \`∫_1^∞ 1/x dx\` | ∞ | No |
| \`∫_1^∞ 1/x² dx\` | 1 | Yes |
| \`∫_1^∞ 1/x^p dx\` | 1/(p-1) | Yes if p > 1 |

### The p-Test

\`∫_1^∞ 1/x^p dx\` converges if and only if **p > 1**.

This is why the harmonic series Σ1/n diverges (p=1), but Σ1/n² converges (p=2).

### Numerical Approach

Integrate to a large finite \`T\`:
\`\`\`
∫_a^∞ f(x) dx ≈ ∫_a^T f(x) dx  (T large)
\`\`\`

The truncation error is roughly \`|∫_T^∞ f(x) dx|\`. For \`1/x²\`, this is \`1/T\`, so T=100000 gives 5-decimal accuracy.

### Your Task

Implement \`double improper_integral(double (*f)(double), double a, double T, int n)\` that approximates \`∫_a^T f(x) dx\` using the midpoint rule.`,

	starterCode: `#include <stdio.h>

double improper_integral(double (*f)(double), double a, double T, int n) {
\tdouble h = (T - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + (i + 0.5) * h);
\t}
\treturn sum * h;
}

double two_inv_cb(double x) { return 2.0 / (x * x * x); }

int main() {
\t/* ∫_1^∞ 2/x³ dx = 1 (approximate with T=200) */
\tprintf("%.4f\\n", improper_integral(two_inv_cb, 1.0, 200.0, 100000));
\treturn 0;
}`,

	solution: `#include <stdio.h>

double improper_integral(double (*f)(double), double a, double T, int n) {
\tdouble h = (T - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += f(a + (i + 0.5) * h);
\t}
\treturn sum * h;
}

double two_inv_cb(double x) { return 2.0 / (x * x * x); }

int main() {
\tprintf("%.4f\\n", improper_integral(two_inv_cb, 1.0, 200.0, 100000));
\treturn 0;
}`,

	tests: [
		{
			name: "∫_1^∞ 2/x³ dx = 1 (via T=200)",
			expected: "1.0000\n",
		},
		{
			name: "∫_1^∞ 1/x³ dx = 0.5 (via T=200)",
			code: `#include <stdio.h>
{{FUNC}}
double inv_cube(double x) { return 1.0 / (x * x * x); }
int main() {
\tprintf("%.4f\\n", improper_integral(inv_cube, 1.0, 200.0, 100000));
\treturn 0;
}`,
			expected: "0.5000\n",
		},
		{
			name: "∫_0^∞ finite constant: ∫_0^5 3 dx = 15",
			code: `#include <stdio.h>
{{FUNC}}
double three(double x) { return 3.0; }
int main() {
\tprintf("%.4f\\n", improper_integral(three, 0.0, 5.0, 1000));
\treturn 0;
}`,
			expected: "15.0000\n",
		},
		{
			name: "∫_1^∞ 1/x^4 dx = 1/3 ≈ 0.3333 (via T=200)",
			code: `#include <stdio.h>
{{FUNC}}
double inv_4(double x) { return 1.0 / (x * x * x * x); }
int main() {
\tprintf("%.4f\\n", improper_integral(inv_4, 1.0, 200.0, 100000));
\treturn 0;
}`,
			expected: "0.3333\n",
		},
	],
};
