import type { Lesson } from "../../types";

export const ratioTest: Lesson = {
	id: "ratio-test",
	title: "Ratio Test",
	chapterId: "sequences-and-series",
	content: `## Ratio Test

For a series \`Σ a_n\`, compute:
\`\`\`
L = lim_{n→∞} |a_{n+1} / a_n|
\`\`\`

- **L < 1**: series converges absolutely
- **L > 1**: series diverges
- **L = 1**: inconclusive (try another test)

### Why It Works

If the ratio approaches \`L < 1\`, the tail of the series eventually behaves like a geometric series with ratio \`L\`, which converges.

### Examples

**Factorial series** \`Σ 1/n!\`:
\`\`\`
|a_{n+1}/a_n| = n!/(n+1)! = 1/(n+1) → 0  (L=0 < 1, converges)
\`\`\`

**Power series** \`Σ xⁿ/n!\` (Taylor series for eˣ):
\`\`\`
|a_{n+1}/a_n| = |x|/(n+1) → 0  (converges for all x)
\`\`\`

**Geometric series** \`Σ rⁿ\`:
\`\`\`
|a_{n+1}/a_n| = |r|  (constant — converges iff |r| < 1)
\`\`\`

**Harmonic** \`Σ 1/n\`:
\`\`\`
|a_{n+1}/a_n| = n/(n+1) → 1  (L=1, inconclusive)
\`\`\`

### Numerically

Compute \`a_n(n+1) / a_n(n)\` for a large \`n\` to approximate \`L\`.

### Your Task

Implement \`double ratio_test_limit(double (*a_n)(int), int n)\` that returns \`a_n(n+1) / a_n(n)\`, approximating the ratio test limit.`,

	starterCode: `#include <stdio.h>

double ratio_test_limit(double (*a_n)(int), int n) {
\treturn a_n(n + 1) / a_n(n);
}

double harmonic(int k) { return 1.0 / (double)k; }

int main() {
\t/* harmonic: ratio at n=100 = 100/101 ~ 0.9901 (L→1, inconclusive) */
\tprintf("%.4f\\n", ratio_test_limit(harmonic, 100));
\treturn 0;
}`,

	solution: `#include <stdio.h>

double ratio_test_limit(double (*a_n)(int), int n) {
\treturn a_n(n + 1) / a_n(n);
}

double harmonic(int k) { return 1.0 / (double)k; }

int main() {
\tprintf("%.4f\\n", ratio_test_limit(harmonic, 100));
\treturn 0;
}`,

	tests: [
		{
			name: "harmonic 1/k at n=100: ratio ≈ 0.9901 (inconclusive, L→1)",
			expected: "0.9901\n",
		},
		{
			name: "geometric 1/2^k at n=10: ratio = 0.5000 (converges)",
			code: `#include <stdio.h>
{{FUNC}}
double geo(int k) {
\tdouble r = 1.0;
\tfor (int i = 0; i < k; i++) r /= 2.0;
\treturn r;
}
int main() {
\tprintf("%.4f\\n", ratio_test_limit(geo, 10));
\treturn 0;
}`,
			expected: "0.5000\n",
		},
		{
			name: "geometric 2^k at n=10: ratio = 2.0000 (diverges)",
			code: `#include <stdio.h>
{{FUNC}}
double pow2(int k) {
\tdouble r = 1.0;
\tfor (int i = 0; i < k; i++) r *= 2.0;
\treturn r;
}
int main() {
\tprintf("%.4f\\n", ratio_test_limit(pow2, 10));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "factorial 1/k! at n=10: ratio = 1/11 ≈ 0.0909 (converges rapidly)",
			code: `#include <stdio.h>
{{FUNC}}
double inv_fact(int k) {
\tdouble f = 1.0;
\tfor (int i = 2; i <= k; i++) f *= (double)i;
\treturn 1.0 / f;
}
int main() {
\tprintf("%.4f\\n", ratio_test_limit(inv_fact, 10));
\treturn 0;
}`,
			expected: "0.0909\n",
		},
	],
};
