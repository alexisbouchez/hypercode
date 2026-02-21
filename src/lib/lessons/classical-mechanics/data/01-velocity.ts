import type { Lesson } from "../../types";

export const velocityLesson: Lesson = {
	id: "velocity",
	title: "Average Velocity",
	chapterId: "kinematics",
	content: `## Velocity and Displacement

**Displacement** is the change in position: $\\Delta x = x_1 - x_0$.

**Average velocity** is displacement divided by elapsed time:

$$v = \\frac{\\Delta x}{\\Delta t} = \\frac{x_1 - x_0}{\\Delta t}$$

Units: metres per second (m/s). Velocity can be negative — it just means motion in the negative direction.

### Examples

| $x_0$ (m) | $x_1$ (m) | $\\Delta t$ (s) | $v$ (m/s) |
|--------|--------|--------|---------|
| 0 | 10 | 2 | **5.0** |
| 100 | 50 | 5 | **−10.0** |
| 0 | 100 | 4 | **25.0** |

### Instantaneous vs Average

Average velocity is measured over a finite interval. As $\\Delta t \\to 0$, it becomes the **instantaneous velocity** — the derivative $dx/dt$. For constant velocity the two are identical.

### Your Task

Implement \`velocity(x0, x1, dt)\` that returns average velocity in m/s.`,

	starterCode: `#include <stdio.h>

double velocity(double x0, double x1, double dt) {
    /* v = (x1 - x0) / dt */
    return 0;
}

int main() {
    printf("%.4f\\n", velocity(0, 10, 2));    /* 5.0000  */
    printf("%.4f\\n", velocity(100, 50, 5));  /* -10.0000 */
    printf("%.4f\\n", velocity(0, 100, 4));   /* 25.0000 */
    return 0;
}
`,

	solution: `#include <stdio.h>

double velocity(double x0, double x1, double dt) {
    return (x1 - x0) / dt;
}

int main() {
    printf("%.4f\\n", velocity(0, 10, 2));    /* 5.0000  */
    printf("%.4f\\n", velocity(100, 50, 5));  /* -10.0000 */
    printf("%.4f\\n", velocity(0, 100, 4));   /* 25.0000 */
    return 0;
}
`,

	tests: [
		{
			name: "10 m in 2 s → 5.0000 m/s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", velocity(0, 10, 2)); return 0; }`,
			expected: "5.0000\n",
		},
		{
			name: "Moving backwards: 100→50 in 5 s → -10.0000 m/s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", velocity(100, 50, 5)); return 0; }`,
			expected: "-10.0000\n",
		},
		{
			name: "0→100 m in 4 s → 25.0000 m/s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", velocity(0, 100, 4)); return 0; }`,
			expected: "25.0000\n",
		},
		{
			name: "5→20 m in 3 s → 5.0000 m/s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", velocity(5, 20, 3)); return 0; }`,
			expected: "5.0000\n",
		},
	],
};
