import type { Lesson } from "../../types";

export const kineticEnergyLesson: Lesson = {
	id: "kinetic-energy",
	title: "Kinetic Energy",
	chapterId: "energy-and-momentum",
	content: `## Kinetic Energy

**Kinetic energy** is the energy an object possesses by virtue of its motion:

\`\`\`
KE = ½mv²
\`\`\`

Units: Joules (J = kg·m²/s²).

### Key Properties

- Always **non-negative** (v² is always ≥ 0)
- Scales with **mass linearly**: doubling mass doubles KE
- Scales with **velocity squared**: doubling speed quadruples KE

### Work-Energy Theorem

The net work done on an object equals its change in kinetic energy:

\`\`\`
W_net = ΔKE = ½mv_f² − ½mv_i²
\`\`\`

This connects forces (work) to motion (energy) without needing to track the path taken.

### Examples

| m (kg) | v (m/s) | KE (J) |
|--------|---------|--------|
| 2 | 10 | **100** |
| 5 | 4 | **40** |
| 1 | 10 | **50** |
| 70 | 10 | **3500** ← 70 kg person at 36 km/h |

### Your Task

Implement \`kineticEnergy(m, v)\` returning kinetic energy in Joules.`,

	starterCode: `#include <stdio.h>

double kineticEnergy(double m, double v) {
    /* KE = 0.5 * m * v^2 */
    return 0;
}

int main() {
    printf("%.4f\\n", kineticEnergy(2, 10));   /* 100.0000  */
    printf("%.4f\\n", kineticEnergy(5, 4));    /* 40.0000   */
    printf("%.4f\\n", kineticEnergy(1, 10));   /* 50.0000   */
    printf("%.4f\\n", kineticEnergy(70, 10));  /* 3500.0000 */
    return 0;
}
`,

	solution: `#include <stdio.h>

double kineticEnergy(double m, double v) {
    return 0.5 * m * v * v;
}

int main() {
    printf("%.4f\\n", kineticEnergy(2, 10));   /* 100.0000  */
    printf("%.4f\\n", kineticEnergy(5, 4));    /* 40.0000   */
    printf("%.4f\\n", kineticEnergy(1, 10));   /* 50.0000   */
    printf("%.4f\\n", kineticEnergy(70, 10));  /* 3500.0000 */
    return 0;
}
`,

	tests: [
		{
			name: "2 kg at 10 m/s → 100.0000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", kineticEnergy(2, 10)); return 0; }`,
			expected: "100.0000\n",
		},
		{
			name: "5 kg at 4 m/s → 40.0000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", kineticEnergy(5, 4)); return 0; }`,
			expected: "40.0000\n",
		},
		{
			name: "1 kg at 10 m/s → 50.0000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", kineticEnergy(1, 10)); return 0; }`,
			expected: "50.0000\n",
		},
		{
			name: "70 kg at 10 m/s → 3500.0000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", kineticEnergy(70, 10)); return 0; }`,
			expected: "3500.0000\n",
		},
	],
};
