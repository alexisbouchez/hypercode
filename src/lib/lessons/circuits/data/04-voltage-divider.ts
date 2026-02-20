import type { Lesson } from "../../types";

export const voltageDivider: Lesson = {
	id: "voltage_div",
	title: "Voltage Divider",
	chapterId: "dc-fundamentals",
	content: `## The Voltage Divider

A **voltage divider** is two series resistors used to produce a fraction of the supply voltage:

\`\`\`
    +Vin
     |
    [R1]
     |
     +---- Vout
     |
    [R2]
     |
    GND
\`\`\`

The output voltage:

\`\`\`
Vout = Vin · R2 / (R1 + R2)
\`\`\`

### Derivation

The same current flows through R1 and R2 (series circuit):

\`\`\`
I = Vin / (R1 + R2)
Vout = I · R2 = Vin · R2 / (R1 + R2)
\`\`\`

### Intuition

The output is a **ratio**: Vout/Vin = R2/(R1+R2). Only the ratio of resistors matters, not their absolute values.

- R1 = R2 → Vout = Vin/2 (splits evenly)
- R2 >> R1 → Vout ≈ Vin (almost no drop on R1)
- R2 << R1 → Vout ≈ 0 (almost all drop on R1)

### Applications

- Setting bias voltages in amplifiers
- Scaling sensor outputs to ADC range
- Level shifting (e.g., 5V → 3.3V logic)

### Warning

A voltage divider is only accurate when the load resistance is much larger than R2. A low-resistance load "steals" current and pulls Vout down.

### Examples

| Vin | R1 | R2 | Vout |
|-----|----|----|------|
| 12V | 10Ω | 10Ω | **6V** |
| 5V | 3Ω | 2Ω | **2V** |
| 9V | 2Ω | 1Ω | **3V** |
| 10V | 3Ω | 7Ω | **7V** |

### Your Task

Implement \`double voltage_divider(double vin, double r1, double r2)\` that returns Vout.`,

	starterCode: `#include <stdio.h>

double voltage_divider(double vin, double r1, double r2) {
\t/* Vout = Vin * R2 / (R1 + R2) */
\treturn 0.0;
}

int main() {
\t/* 12V with equal resistors → 6V */
\tprintf("%.4f\\n", voltage_divider(12.0, 10.0, 10.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double voltage_divider(double vin, double r1, double r2) {
\treturn vin * r2 / (r1 + r2);
}

int main() {
\tprintf("%.4f\\n", voltage_divider(12.0, 10.0, 10.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "12V, R1=R2=10Ω → 6V",
			expected: "6.0000\n",
		},
		{
			name: "5V, R1=3Ω, R2=2Ω → 2V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", voltage_divider(5.0, 3.0, 2.0));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "9V, R1=2Ω, R2=1Ω → 3V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", voltage_divider(9.0, 2.0, 1.0));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "10V, R1=3Ω, R2=7Ω → 7V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", voltage_divider(10.0, 3.0, 7.0));
\treturn 0;
}`,
			expected: "7.0000\n",
		},
	],
};
