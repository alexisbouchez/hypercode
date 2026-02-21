import type { Lesson } from "../../types";

export const torqueLesson: Lesson = {
	id: "torque",
	title: "Torque",
	chapterId: "oscillations-and-gravitation",
	content: `## Torque

**Torque** is the rotational analogue of force. It measures how effectively a force causes rotation about a pivot:

$$\\tau = F r \\sin(\\theta)$$

- $F$ — applied force (N)
- $r$ — moment arm — distance from pivot to point of application (m)
- $\\theta$ — angle between force vector and the moment arm
- $\\tau$ — torque (N·m)

### When is torque maximum?

When $\\theta = 90°$ (force perpendicular to the arm): $\\sin(90°) = 1$, so $\\tau = Fr$.

Pushing a door at the handle (far from hinge, $\\theta = 90°$) is far more effective than pushing near the hinge or pushing at an angle.

### Newton's 2nd Law for Rotation

$$\\tau_{\\text{net}} = I \\alpha$$

where $I$ is the moment of inertia and $\\alpha$ is angular acceleration — the rotational analogue of $F = ma$.

### Examples

| $F$ (N) | $r$ (m) | $\\theta$ | $\\tau$ (N·m) |
|-------|-------|---|---------|
| 10 | 2 | 90° | **20.0000** |
| 10 | 2 | 30° | **10.0000** ($\\sin 30° = 0.5$) |
| 50 | 3 | 90° | **150.0000** |
| 100 | 0.5 | 45° | **35.3553** |

### Your Task

Implement \`torque(F, r, angle_deg)\` returning torque in N·m.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double torque(double F, double r, double angle_deg) {
    /* tau = F * r * sin(angle_rad) */
    return 0;
}

int main() {
    printf("%.4f\\n", torque(10, 2, 90));     /* 20.0000  */
    printf("%.4f\\n", torque(10, 2, 30));     /* 10.0000  */
    printf("%.4f\\n", torque(50, 3, 90));     /* 150.0000 */
    printf("%.4f\\n", torque(100, 0.5, 45));  /* 35.3553  */
    return 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double torque(double F, double r, double angle_deg) {
    double angle = angle_deg * PI / 180.0;
    return F * r * sin(angle);
}

int main() {
    printf("%.4f\\n", torque(10, 2, 90));     /* 20.0000  */
    printf("%.4f\\n", torque(10, 2, 30));     /* 10.0000  */
    printf("%.4f\\n", torque(50, 3, 90));     /* 150.0000 */
    printf("%.4f\\n", torque(100, 0.5, 45));  /* 35.3553  */
    return 0;
}
`,

	tests: [
		{
			name: "F=10 N, r=2 m, θ=90° → 20.0000 N·m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", torque(10, 2, 90)); return 0; }`,
			expected: "20.0000\n",
		},
		{
			name: "F=10 N, r=2 m, θ=30° → 10.0000 N·m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", torque(10, 2, 30)); return 0; }`,
			expected: "10.0000\n",
		},
		{
			name: "F=50 N, r=3 m, θ=90° → 150.0000 N·m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", torque(50, 3, 90)); return 0; }`,
			expected: "150.0000\n",
		},
		{
			name: "F=100 N, r=0.5 m, θ=45° → 35.3553 N·m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", torque(100, 0.5, 45)); return 0; }`,
			expected: "35.3553\n",
		},
	],
};
