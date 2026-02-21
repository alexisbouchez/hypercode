import type { Lesson } from "../../types";

export const shmDisplacementLesson: Lesson = {
	id: "shm-displacement",
	title: "Simple Harmonic Motion",
	chapterId: "oscillations-and-gravitation",
	content: `## Simple Harmonic Motion

**Simple Harmonic Motion (SHM)** occurs when a restoring force is proportional to displacement (Hooke's law: $F = -kx$). The resulting motion is sinusoidal:

$$x(t) = A \\cos(\\omega t)$$

- $A$ — amplitude (maximum displacement, metres)
- $\\omega$ — angular frequency (rad/s): $\\omega = 2\\pi f = 2\\pi / T$
- $t$ — time (seconds)

Starting at $t = 0$, the object is at maximum displacement $x = A$.

### Key Quantities

$$T = \\frac{2\\pi}{\\omega} \qquad f = \\frac{\\omega}{2\\pi}$$

### Velocity and Acceleration in SHM

$$v(t) = -A\\omega \\sin(\\omega t)$$

$$a(t) = -A\\omega^2 \\cos(\\omega t) = -\\omega^2 x$$

The acceleration always opposes displacement — this is the hallmark of SHM.

### Examples

| $A$ | $\\omega$ | $t$ | $x(t)$ |
|---|---|---|------|
| 1 | 1 | 0 | **1.0000** (at maximum) |
| 5 | 2 | 0 | **5.0000** |
| 3 | 1 | $\\pi$ | **−3.0000** (at minimum) |
| 10 | 1 | $2\\pi$ | **10.0000** (back to start) |

### Your Task

Implement \`shmX(A, omega, t)\` returning displacement in metres.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double shmX(double A, double omega, double t) {
    /* x = A * cos(omega * t) */
    return 0;
}

int main() {
    printf("%.4f\\n", shmX(1, 1, 0));           /* 1.0000  (at start) */
    printf("%.4f\\n", shmX(5, 2, 0));           /* 5.0000  */
    printf("%.4f\\n", shmX(3, 1, 3.14159));     /* -3.0000 (at minimum) */
    printf("%.4f\\n", shmX(10, 1, 6.28318));    /* 10.0000 (full cycle) */
    return 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double shmX(double A, double omega, double t) {
    return A * cos(omega * t);
}

int main() {
    printf("%.4f\\n", shmX(1, 1, 0));           /* 1.0000  */
    printf("%.4f\\n", shmX(5, 2, 0));           /* 5.0000  */
    printf("%.4f\\n", shmX(3, 1, 3.14159));     /* -3.0000 */
    printf("%.4f\\n", shmX(10, 1, 6.28318));    /* 10.0000 */
    return 0;
}
`,

	tests: [
		{
			name: "A=1, ω=1, t=0 → 1.0000 (at maximum)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", shmX(1, 1, 0)); return 0; }`,
			expected: "1.0000\n",
		},
		{
			name: "A=5, ω=2, t=0 → 5.0000",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", shmX(5, 2, 0)); return 0; }`,
			expected: "5.0000\n",
		},
		{
			name: "A=3, ω=1, t=π → -3.0000 (at minimum)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", shmX(3, 1, 3.14159)); return 0; }`,
			expected: "-3.0000\n",
		},
		{
			name: "A=10, ω=1, t=2π → 10.0000 (full cycle)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", shmX(10, 1, 6.28318)); return 0; }`,
			expected: "10.0000\n",
		},
	],
};
