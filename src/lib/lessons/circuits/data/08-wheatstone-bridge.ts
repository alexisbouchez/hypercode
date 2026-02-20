import type { Lesson } from "../../types";

export const wheatstoneBridge: Lesson = {
	id: "wheatstone",
	title: "Wheatstone Bridge",
	chapterId: "circuit-analysis",
	content: `## The Wheatstone Bridge

A **Wheatstone bridge** is a classic circuit for measuring unknown resistances with high precision. It consists of four resistors arranged in a diamond:

\`\`\`
        +Vs
       /    \\
     [R1]  [R2]
      |      |
     (A)    (B)   ← Vout = VA - VB
      |      |
     [R3]  [R4]
       \\    /
        GND
\`\`\`

The output voltage:

\`\`\`
Vout = Vs · [R3/(R1+R3) − R4/(R2+R4)]
\`\`\`

### Balanced vs. Unbalanced

The bridge is **balanced** (Vout = 0) when:

\`\`\`
R1/R3 = R2/R4    (or equivalently: R1·R4 = R2·R3)
\`\`\`

In practice: fix R1, R2, R3 to known values, then adjust R4 until Vout = 0. At balance, R4 = R3·R2/R1.

### Why It's Powerful

At balance, the measurement is **independent of Vs** — supply fluctuations don't affect the result. This makes it far more accurate than a simple voltage divider measurement.

### Applications

- Strain gauges (measuring force/deformation)
- Precision thermometers (RTDs)
- Pressure sensors
- Any resistive sensor needing high precision

### Examples

| Vs | R1 | R2 | R3 | R4 | Vout |
|----|----|----|----|----|------|
| 10V | 10Ω | 10Ω | 10Ω | 10Ω | **0V** (balanced) |
| 12V | 2Ω | 6Ω | 6Ω | 2Ω | **6V** |
| 12V | 3Ω | 3Ω | 3Ω | 6Ω | **-2V** |

### Your Task

Implement \`double wheatstone_bridge(double vs, double r1, double r2, double r3, double r4)\` that returns Vout = VA − VB.`,

	starterCode: `#include <stdio.h>

double wheatstone_bridge(double vs, double r1, double r2,
                          double r3, double r4) {
\t/* Vout = Vs * (R3/(R1+R3) - R4/(R2+R4)) */
\treturn 0.0;
}

int main() {
\t/* Balanced: all 10Ω → 0V */
\tprintf("%.4f\\n", wheatstone_bridge(10.0, 10.0,10.0,10.0,10.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double wheatstone_bridge(double vs, double r1, double r2,
                          double r3, double r4) {
\tdouble va = vs * r3 / (r1 + r3);
\tdouble vb = vs * r4 / (r2 + r4);
\treturn va - vb;
}

int main() {
\tprintf("%.4f\\n", wheatstone_bridge(10.0, 10.0,10.0,10.0,10.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "balanced: all 10Ω → 0V",
			expected: "0.0000\n",
		},
		{
			name: "Vs=12, R1=2,R2=6,R3=6,R4=2 → +6V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", wheatstone_bridge(12.0, 2.0,6.0,6.0,2.0));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
		{
			name: "Vs=12, R1=3,R2=3,R3=3,R4=6 → -2V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", wheatstone_bridge(12.0, 3.0,3.0,3.0,6.0));
\treturn 0;
}`,
			expected: "-2.0000\n",
		},
		{
			name: "Vs=10, ratio match R1/R3=R2/R4=0.5 → 0V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", wheatstone_bridge(10.0, 1.0,2.0,2.0,4.0));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
