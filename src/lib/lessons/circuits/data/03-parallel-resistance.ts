import type { Lesson } from "../../types";

export const parallelResistance: Lesson = {
	id: "parallel_r",
	title: "Parallel Resistance",
	chapterId: "dc-fundamentals",
	content: `## Resistors in Parallel

When resistors share the **same two nodes**, they are in parallel. Current splits among them.

### The Formula

\`\`\`
1/R_total = 1/R₁ + 1/R₂ + ... + 1/Rₙ
\`\`\`

Or equivalently, summing the **conductances** (G = 1/R):

\`\`\`
G_total = G₁ + G₂ + ... + Gₙ
R_total  = 1 / G_total
\`\`\`

### For Two Resistors

There's a handy shortcut:

\`\`\`
R_total = (R₁ · R₂) / (R₁ + R₂)    ("product over sum")
\`\`\`

### Key Properties

- **Same voltage** across every parallel resistor
- **Currents add** to give total current: I_total = I₁ + I₂ + ...
- Total resistance is **always less** than the smallest individual resistor
- Two equal resistors in parallel → half their value

### Examples

| Resistors | Total |
|-----------|-------|
| 10Ω \|\| 10Ω | **5Ω** |
| 6Ω \|\| 3Ω | **2Ω** |
| 100Ω \|\| 100Ω \|\| 100Ω | **33.3333Ω** |

### Your Task

Implement \`double parallel_resistance(double *r, int n)\` that returns the total parallel resistance.`,

	starterCode: `#include <stdio.h>

double parallel_resistance(double *r, int n) {
\t/* 1/R_total = 1/R1 + 1/R2 + ... */
\treturn 0.0;
}

int main() {
\tdouble r[] = {10.0, 10.0};
\tprintf("%.4f\\n", parallel_resistance(r, 2));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double parallel_resistance(double *r, int n) {
\tdouble g = 0.0;
\tfor (int i = 0; i < n; i++) g += 1.0 / r[i];
\treturn 1.0 / g;
}

int main() {
\tdouble r[] = {10.0, 10.0};
\tprintf("%.4f\\n", parallel_resistance(r, 2));
\treturn 0;
}
`,

	tests: [
		{
			name: "10Ω || 10Ω = 5Ω",
			expected: "5.0000\n",
		},
		{
			name: "6Ω || 3Ω = 2Ω",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble r[] = {6.0, 3.0};
\tprintf("%.4f\\n", parallel_resistance(r, 2));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "100Ω || 100Ω || 100Ω = 33.3333Ω",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble r[] = {100.0, 100.0, 100.0};
\tprintf("%.4f\\n", parallel_resistance(r, 3));
\treturn 0;
}`,
			expected: "33.3333\n",
		},
		{
			name: "4Ω || 4Ω = 2Ω",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble r[] = {4.0, 4.0};
\tprintf("%.4f\\n", parallel_resistance(r, 2));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
	],
};
