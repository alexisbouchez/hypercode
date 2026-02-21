import type { Lesson } from "../../types";

export const pendulumPeriodLesson: Lesson = {
	id: "pendulum-period",
	title: "Pendulum Period",
	chapterId: "oscillations-and-gravitation",
	content: `## The Simple Pendulum

A mass on a string of length $L$, swinging under gravity, oscillates with period:

$$T = 2\\pi\\sqrt{\\frac{L}{g}}$$

where $g = 9.81$ m/s².

### Historical Importance

Galileo noticed that pendulum period is independent of amplitude (for small angles). This led to pendulum clocks — the most accurate timekeepers for 300 years, from Huygens (1656) to quartz oscillators (1920s).

### The Small-Angle Approximation

This formula holds when the angle is small ($\\lesssim 15°$). For large angles, the true period is longer and requires an elliptic integral to compute exactly.

### Period vs Spring-Mass

| | Spring-Mass | Pendulum |
|--|------------|---------|
| Formula | $2\\pi\\sqrt{m/k}$ | $2\\pi\\sqrt{L/g}$ |
| Restoring force | Spring: $F = kx$ | Gravity: $F \\approx mg\\theta$ |
| Effective "k" | $k$ | $mg/L$ |

For the pendulum, the "spring constant" is $mg/L$ — so mass cancels out and period is mass-independent.

### Examples

| $L$ (m) | $T$ (s) |
|-------|-------|
| 1.000 | **2.0061** |
| 4.000 | **4.0122** |
| 9.810 | **6.2832** $= 2\\pi$ |
| 0.248 | **0.9990** $\\approx 1$ s (a seconds pendulum) |

### Your Task

Implement \`pendulumPeriod(L)\` returning the period in seconds.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define G  9.81
#define PI 3.14159265358979

double pendulumPeriod(double L) {
    /* T = 2*PI * sqrt(L / G) */
    return 0;
}

int main() {
    printf("%.4f\\n", pendulumPeriod(1));      /* 2.0061 */
    printf("%.4f\\n", pendulumPeriod(4));      /* 4.0121 */
    printf("%.4f\\n", pendulumPeriod(9.81));   /* 6.2832 */
    printf("%.4f\\n", pendulumPeriod(0.25));   /* 1.0030 */
    return 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define G  9.81
#define PI 3.14159265358979

double pendulumPeriod(double L) {
    return 2.0 * PI * sqrt(L / G);
}

int main() {
    printf("%.4f\\n", pendulumPeriod(1));      /* 2.0061 */
    printf("%.4f\\n", pendulumPeriod(4));      /* 4.0121 */
    printf("%.4f\\n", pendulumPeriod(9.81));   /* 6.2832 */
    printf("%.4f\\n", pendulumPeriod(0.25));   /* 1.0030 */
    return 0;
}
`,

	tests: [
		{
			name: "L=1 m → 2.0061 s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", pendulumPeriod(1)); return 0; }`,
			expected: "2.0061\n",
		},
		{
			name: "L=4 m → 4.0121 s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", pendulumPeriod(4)); return 0; }`,
			expected: "4.0121\n",
		},
		{
			name: "L=9.81 m → 6.2832 s (= 2π)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", pendulumPeriod(9.81)); return 0; }`,
			expected: "6.2832\n",
		},
		{
			name: "L=0.25 m → 1.0030 s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", pendulumPeriod(0.25)); return 0; }`,
			expected: "1.0030\n",
		},
	],
};
