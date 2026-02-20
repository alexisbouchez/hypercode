import type { Lesson } from "../../types";

export const geometricSeries: Lesson = {
	id: "geometric-series",
	title: "Geometric Series",
	chapterId: "sequences-and-series",
	content: `## Geometric Series

A **geometric series** has each term a constant multiple of the previous:

\`\`\`
Σ_{k=0}^{n-1} a·rᵏ = a + ar + ar² + ... + ar^{n-1}
\`\`\`

### Closed-Form Formula

For \`r ≠ 1\`:
\`\`\`
S_n = a · (1 - rⁿ) / (1 - r)
\`\`\`

### Convergence

The infinite geometric series \`Σ a·rᵏ\` converges if and only if \`|r| < 1\`:
\`\`\`
Σ_{k=0}^∞ a·rᵏ = a / (1 - r)  when |r| < 1
\`\`\`

| r | Series behavior |
|---|-----------------|
| \`r = 0.5\` | converges to \`2a\` |
| \`r = 1\` | diverges (grows linearly) |
| \`r = 2\` | diverges (grows exponentially) |
| \`r = -1\` | oscillates (diverges) |

### Example

\`a = 1, r = 0.5, n = 10\`:
\`\`\`
S_10 = 1 + 0.5 + 0.25 + ... + 1/512 = 1·(1 - 0.5^10)/(1 - 0.5)
     = 2·(1 - 1/1024) ≈ 1.9980
\`\`\`

The infinite sum: \`1/(1-0.5) = 2\` — the partial sum already reached \`1.998\`.

### Your Task

Implement \`double geometric_partial(double a, double r, int n)\` that computes \`Σ_{k=0}^{n-1} a·rᵏ\` using a loop.`,

	starterCode: `#include <stdio.h>

double geometric_partial(double a, double r, int n) {
\tdouble sum = 0.0;
\tdouble term = a;
\tfor (int k = 0; k < n; k++) {
\t\tsum += term;
\t\tterm *= r;
\t}
\treturn sum;
}

int main() {
\t/* a=1, r=0.5, n=10 ~ 1.9980 */
\tprintf("%.4f\\n", geometric_partial(1.0, 0.5, 10));
\treturn 0;
}`,

	solution: `#include <stdio.h>

double geometric_partial(double a, double r, int n) {
\tdouble sum = 0.0;
\tdouble term = a;
\tfor (int k = 0; k < n; k++) {
\t\tsum += term;
\t\tterm *= r;
\t}
\treturn sum;
}

int main() {
\tprintf("%.4f\\n", geometric_partial(1.0, 0.5, 10));
\treturn 0;
}`,

	tests: [
		{
			name: "a=1, r=0.5, n=10 ≈ 1.9980",
			expected: "1.9980\n",
		},
		{
			name: "a=1, r=0.5, n=1 = 1.0000 (first term only)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", geometric_partial(1.0, 0.5, 1));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "a=3, r=0.25, n=4 = 3(1+0.25+0.0625+0.015625) ≈ 3.9844",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", geometric_partial(3.0, 0.25, 4));
\treturn 0;
}`,
			expected: "3.9844\n",
		},
		{
			name: "a=1, r=2, n=3 = 1+2+4 = 7.0000 (diverging)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", geometric_partial(1.0, 2.0, 3));
\treturn 0;
}`,
			expected: "7.0000\n",
		},
	],
};
