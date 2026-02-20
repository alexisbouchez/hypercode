import type { Lesson } from "../../types";

export const capacitorEnergy: Lesson = {
	id: "cap_energy",
	title: "Energy in Capacitor & Inductor",
	chapterId: "transient-response",
	content: `## Stored Energy

Capacitors and inductors don't dissipate energy like resistors — they **store** it.

### Capacitor

A capacitor stores energy in the electric field between its plates:

\`\`\`
E_C = ½ · C · V²
\`\`\`

Units: farads × volts² = joules

### Inductor

An inductor stores energy in its magnetic field:

\`\`\`
E_L = ½ · L · I²
\`\`\`

Units: henries × amperes² = joules

### Total Energy in RLC Circuit

The total stored energy at any instant:

\`\`\`
E_total = ½·C·V_C² + ½·L·I²
\`\`\`

In an underdamped RLC circuit, energy oscillates between the capacitor and inductor, gradually dissipated by R.

### Practical Examples

| Component | Value | Condition | Energy |
|-----------|-------|-----------|--------|
| Capacitor | 1 mF | 5V | **12.5 mJ** |
| Capacitor | 1 mF | 10V | **50 mJ** |
| Inductor | 1 H | 2A | **2 J** |
| Capacitor | 100 μF | 100V | **0.5 J** |

Note: energy scales as **V²** for capacitors — doubling the voltage stores 4× the energy.

### Applications

- Camera flash: large capacitor discharged quickly through a xenon tube
- Power supply filtering: capacitor releases stored energy during current spikes
- Inductive kickback: inductor's stored energy causes voltage spike when current is interrupted

### Your Task

Implement \`double stored_energy(double c, double vc, double l, double il)\` that returns the total stored energy E = ½·C·V_C² + ½·L·I_L².`,

	starterCode: `#include <stdio.h>

double stored_energy(double c, double vc, double l, double il) {
\t/* E = 0.5*C*Vc^2 + 0.5*L*Il^2 */
\treturn 0.0;
}

int main() {
\t/* C=0.001F, Vc=5V, no inductor → 12.5mJ */
\tprintf("%.4f\\n", stored_energy(0.001, 5.0, 0.0, 0.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double stored_energy(double c, double vc, double l, double il) {
\treturn 0.5*c*vc*vc + 0.5*l*il*il;
}

int main() {
\tprintf("%.4f\\n", stored_energy(0.001, 5.0, 0.0, 0.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "C=1mF at 5V → 12.5mJ = 0.0125J",
			expected: "0.0125\n",
		},
		{
			name: "C=1mF at 10V → 50mJ = 0.0500J",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", stored_energy(0.001, 10.0, 0.0, 0.0));
\treturn 0;
}`,
			expected: "0.0500\n",
		},
		{
			name: "L=1H at 2A → 2J",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", stored_energy(0.0, 0.0, 1.0, 2.0));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "combined: C=1F at 2V + L=1H at 2A = 2+2 = 4J",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", stored_energy(1.0, 2.0, 1.0, 2.0));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
	],
};
