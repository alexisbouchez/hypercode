import type { Lesson } from "../../types";

export const currentDivider: Lesson = {
	id: "current_div",
	title: "Current Divider",
	chapterId: "circuit-analysis",
	content: `## The Current Divider

A **current divider** is two parallel resistors: total current splits between them.

\`\`\`
     +---[R1]---+
I →  |          | → I
     +---[R2]---+
\`\`\`

The current through R1:

\`\`\`
I1 = I_total · R2 / (R1 + R2)
\`\`\`

Notice: **the other resistor** (R2) appears in the numerator.

### Why the Opposite Resistor?

Both branches share the same voltage V. By Ohm's law:
- I1 = V / R1
- I2 = V / R2
- I_total = V · (1/R1 + 1/R2) = V · (R1+R2)/(R1·R2)

Solving for I1:

\`\`\`
I1 = I_total · R2/(R1+R2)
\`\`\`

The lower the resistance in a branch, the **more** current it takes. The higher-resistance branch appears in the numerator because a larger R2 means R1's branch gets more.

### Symmetry

| Path | Current |
|------|---------|
| Through R1 | I_total · R2/(R1+R2) |
| Through R2 | I_total · R1/(R1+R2) |

The two always sum to I_total. ✓

### Examples

| I_total | R1 | R2 | I₁ |
|---------|----|----|----|
| 6A | 2Ω | 3Ω | **3.6A** |
| 10A | 5Ω | 5Ω | **5A** (split evenly) |
| 12A | 3Ω | 6Ω | **8A** |

### Your Task

Implement \`double current_divider(double itotal, double r1, double r2)\` that returns the current through R1.`,

	starterCode: `#include <stdio.h>

double current_divider(double itotal, double r1, double r2) {
\t/* I1 = Itotal * R2 / (R1 + R2) */
\treturn 0.0;
}

int main() {
\t/* 6A total, R1=2Ω, R2=3Ω → I1 = 3.6A */
\tprintf("%.4f\\n", current_divider(6.0, 2.0, 3.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double current_divider(double itotal, double r1, double r2) {
\treturn itotal * r2 / (r1 + r2);
}

int main() {
\tprintf("%.4f\\n", current_divider(6.0, 2.0, 3.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "6A, R1=2Ω, R2=3Ω → I1=3.6A",
			expected: "3.6000\n",
		},
		{
			name: "10A equal resistors → I1=5A",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", current_divider(10.0, 5.0, 5.0));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
		{
			name: "12A, R1=3Ω, R2=6Ω → I1=8A",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", current_divider(12.0, 3.0, 6.0));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
		{
			name: "1A, R1=1Ω, R2=3Ω → I1=0.75A",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", current_divider(1.0, 1.0, 3.0));
\treturn 0;
}`,
			expected: "0.7500\n",
		},
	],
};
