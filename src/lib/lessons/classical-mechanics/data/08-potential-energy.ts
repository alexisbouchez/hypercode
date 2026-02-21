import type { Lesson } from "../../types";

export const potentialEnergyLesson: Lesson = {
	id: "potential-energy",
	title: "Gravitational Potential Energy",
	chapterId: "energy-and-momentum",
	content: `## Gravitational Potential Energy

An object at height $h$ above a reference level has **gravitational potential energy**:

$$PE = mgh$$

- $m$ — mass in kg
- $g$ — 9.81 m/s² (gravitational acceleration)
- $h$ — height above reference level in metres

### Conservation of Energy

In the absence of friction, mechanical energy is conserved:

$$KE + PE = \\text{constant}$$

$$\\frac{1}{2}mv^2 + mgh = \\text{constant}$$

This means PE converts to KE as an object falls, and vice versa as it rises. A ball dropped from height $h$ reaches the ground with speed:

$$v = \\sqrt{2gh}$$

### Examples

| $m$ (kg) | $h$ (m) | PE (J) |
|--------|-------|--------|
| 10 | 5 | **490.5** |
| 1 | 10 | **98.1** |
| 5 | 20 | **981.0** |
| 70 | 2 | **1373.4** |

### Your Task

Implement \`gravPE(m, h)\` returning gravitational potential energy in Joules.`,

	starterCode: `#include <stdio.h>

#define G 9.81

double gravPE(double m, double h) {
    /* PE = m * G * h */
    return 0;
}

int main() {
    printf("%.4f\\n", gravPE(10, 5));   /* 490.5000  */
    printf("%.4f\\n", gravPE(1, 10));   /* 98.1000   */
    printf("%.4f\\n", gravPE(5, 20));   /* 981.0000  */
    printf("%.4f\\n", gravPE(70, 2));   /* 1373.4000 */
    return 0;
}
`,

	solution: `#include <stdio.h>

#define G 9.81

double gravPE(double m, double h) {
    return m * G * h;
}

int main() {
    printf("%.4f\\n", gravPE(10, 5));   /* 490.5000  */
    printf("%.4f\\n", gravPE(1, 10));   /* 98.1000   */
    printf("%.4f\\n", gravPE(5, 20));   /* 981.0000  */
    printf("%.4f\\n", gravPE(70, 2));   /* 1373.4000 */
    return 0;
}
`,

	tests: [
		{
			name: "10 kg at 5 m → 490.5000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravPE(10, 5)); return 0; }`,
			expected: "490.5000\n",
		},
		{
			name: "1 kg at 10 m → 98.1000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravPE(1, 10)); return 0; }`,
			expected: "98.1000\n",
		},
		{
			name: "5 kg at 20 m → 981.0000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravPE(5, 20)); return 0; }`,
			expected: "981.0000\n",
		},
		{
			name: "70 kg at 2 m → 1373.4000 J",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravPE(70, 2)); return 0; }`,
			expected: "1373.4000\n",
		},
	],
};
