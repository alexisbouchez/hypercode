import type { Lesson } from "../../types";

export const powerDissipated: Lesson = {
	id: "power_r",
	title: "Power Dissipation",
	chapterId: "circuit-analysis",
	content: `## Power in a Resistor

When current flows through a resistor, electrical energy is converted to heat. The **power dissipated** is:

\`\`\`
P = I² · R
\`\`\`

Using Ohm's law (V = IR), there are three equivalent forms:

\`\`\`
P = I² · R       (use when you know I and R)
P = V² / R       (use when you know V and R)
P = V · I        (use when you know V and I)
\`\`\`

### Unit: Watt (W)

1 Watt = 1 Joule per second = 1 Volt × 1 Ampere

### Resistor Ratings

Resistors have a **power rating** — the maximum power they can safely dissipate (commonly 1/8W, 1/4W, 1/2W, 1W). Exceed it and they fail or catch fire.

Before choosing a resistor, always verify:

\`\`\`
P_calculated < P_rating
\`\`\`

With a 2× safety margin is good practice.

### Examples

| Current (A) | Resistance (Ω) | Power (W) |
|-------------|----------------|-----------|
| 2A | 5Ω | **20W** |
| 1A | 10Ω | **10W** |
| 3A | 4Ω | **36W** |
| 0.1A | 100Ω | **1W** |

### Energy Over Time

If the power is constant, energy consumed in time t:

\`\`\`
E = P · t    (joules, if P in watts and t in seconds)
\`\`\`

### Your Task

Implement \`double power_dissipated(double i, double r)\` that returns the power dissipated as P = I² · R.`,

	starterCode: `#include <stdio.h>

double power_dissipated(double i, double r) {
\t/* P = I^2 * R */
\treturn 0.0;
}

int main() {
\t/* 2A through 5Ω → 20W */
\tprintf("%.4f\\n", power_dissipated(2.0, 5.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double power_dissipated(double i, double r) {
\treturn i * i * r;
}

int main() {
\tprintf("%.4f\\n", power_dissipated(2.0, 5.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "I=2A, R=5Ω → P=20W",
			expected: "20.0000\n",
		},
		{
			name: "I=1A, R=10Ω → P=10W",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", power_dissipated(1.0, 10.0));
\treturn 0;
}`,
			expected: "10.0000\n",
		},
		{
			name: "I=3A, R=4Ω → P=36W",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", power_dissipated(3.0, 4.0));
\treturn 0;
}`,
			expected: "36.0000\n",
		},
		{
			name: "I=0.1A, R=100Ω → P=1W",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", power_dissipated(0.1, 100.0));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
	],
};
