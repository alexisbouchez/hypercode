import type { Lesson } from "../../types";

export const ohmsLaw: Lesson = {
	id: "ohm",
	title: "Ohm's Law",
	chapterId: "dc-fundamentals",
	content: `## Ohm's Law

**Ohm's Law** is the most fundamental relationship in circuit analysis:

\`\`\`
V = I · R
\`\`\`

Where:
- **V** = voltage across the resistor (volts, V)
- **I** = current through the resistor (amperes, A)
- **R** = resistance (ohms, Ω)

### The Three Forms

Rearranging gives you the form you need:

\`\`\`
I = V / R     (find current from voltage and resistance)
V = I · R     (find voltage from current and resistance)
R = V / I     (find resistance from voltage and current)
\`\`\`

### Physical Intuition

Think of a resistor as a pipe carrying water:
- **Voltage** is the pressure pushing the water
- **Current** is the flow rate
- **Resistance** is how narrow the pipe is

Double the pressure (voltage) → double the flow (current).
Double the pipe restriction (resistance) → half the flow.

### Examples

| Voltage (V) | Resistance (Ω) | Current (A) |
|-------------|----------------|-------------|
| 12 V | 4 Ω | **3 A** |
| 5 V | 1000 Ω | **0.005 A** (5 mA) |
| 9 V | 3 Ω | **3 A** |
| 120 V | 60 Ω | **2 A** |

### Your Task

Implement \`double ohms_law(double v, double r)\` that returns the current through a resistor given voltage \`v\` and resistance \`r\`.`,

	starterCode: `#include <stdio.h>

double ohms_law(double v, double r) {
\t/* return I = V / R */
\treturn 0.0;
}

int main() {
\t/* 12V across 4Ω → 3A */
\tprintf("%.4f\\n", ohms_law(12.0, 4.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double ohms_law(double v, double r) {
\treturn v / r;
}

int main() {
\tprintf("%.4f\\n", ohms_law(12.0, 4.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "12V / 4Ω = 3A",
			expected: "3.0000\n",
		},
		{
			name: "5V / 1000Ω = 0.005A (5mA)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", ohms_law(5.0, 1000.0));
\treturn 0;
}`,
			expected: "0.0050\n",
		},
		{
			name: "9V / 3Ω = 3A",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", ohms_law(9.0, 3.0));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "100V / 50Ω = 2A",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", ohms_law(100.0, 50.0));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
	],
};
