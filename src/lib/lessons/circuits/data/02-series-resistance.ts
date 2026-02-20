import type { Lesson } from "../../types";

export const seriesResistance: Lesson = {
	id: "series_r",
	title: "Series Resistance",
	chapterId: "dc-fundamentals",
	content: `## Resistors in Series

When resistors are connected **end-to-end**, the same current flows through all of them. The total resistance is simply the sum:

\`\`\`
R_total = R₁ + R₂ + R₃ + ... + Rₙ
\`\`\`

### Why It's Additive

Each resistor "opposes" the current. String them together and their opposition stacks up — like connecting multiple narrow pipes end-to-end.

### Key Properties

- **Same current** through every series resistor: I₁ = I₂ = ... = I
- **Voltages add up** to the supply: V₁ + V₂ + ... = V_supply
- Total resistance is **always greater** than any individual resistor

### Examples

| Resistors | Total |
|-----------|-------|
| 10Ω + 20Ω + 30Ω | **60Ω** |
| 100Ω + 200Ω | **300Ω** |
| 47Ω + 33Ω + 10Ω | **90Ω** |

### Practical Note

Series circuits are used in Christmas lights (old style) and current-limiting resistors. If one series element fails open, the entire circuit breaks.

### Your Task

Implement \`double series_resistance(double *r, int n)\` that returns the total series resistance of \`n\` resistors stored in array \`r\`.`,

	starterCode: `#include <stdio.h>

double series_resistance(double *r, int n) {
\t/* return R1 + R2 + ... + Rn */
\treturn 0.0;
}

int main() {
\tdouble r[] = {10.0, 20.0, 30.0};
\tprintf("%.4f\\n", series_resistance(r, 3));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double series_resistance(double *r, int n) {
\tdouble total = 0.0;
\tfor (int i = 0; i < n; i++) total += r[i];
\treturn total;
}

int main() {
\tdouble r[] = {10.0, 20.0, 30.0};
\tprintf("%.4f\\n", series_resistance(r, 3));
\treturn 0;
}
`,

	tests: [
		{
			name: "10+20+30 = 60Ω",
			expected: "60.0000\n",
		},
		{
			name: "100+200 = 300Ω",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble r[] = {100.0, 200.0};
\tprintf("%.4f\\n", series_resistance(r, 2));
\treturn 0;
}`,
			expected: "300.0000\n",
		},
		{
			name: "47+33+10 = 90Ω",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble r[] = {47.0, 33.0, 10.0};
\tprintf("%.4f\\n", series_resistance(r, 3));
\treturn 0;
}`,
			expected: "90.0000\n",
		},
		{
			name: "1+1+1+1 = 4Ω",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble r[] = {1.0, 1.0, 1.0, 1.0};
\tprintf("%.4f\\n", series_resistance(r, 4));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
	],
};
