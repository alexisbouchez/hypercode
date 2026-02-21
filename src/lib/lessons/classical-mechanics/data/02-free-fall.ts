import type { Lesson } from "../../types";

export const freeFallLesson: Lesson = {
	id: "free-fall",
	title: "Free Fall",
	chapterId: "kinematics",
	content: `## Free Fall Under Gravity

Near Earth's surface, gravity accelerates every object downward at:

$$g = 9.81 \\ \\text{m/s}^2$$

For an object with initial downward velocity $v_0$, the distance fallen after time $t$ is:

$$h(t) = v_0 t + \\frac{1}{2}g t^2$$

The first term is the constant-velocity contribution. The second term is the quadratic acceleration term — it grows as $t^2$.

### Examples

| $v_0$ (m/s) | $t$ (s) | $h$ (m) |
|----------|-------|-------|
| 0 | 1 | 4.905 |
| 0 | 2 | 19.62 |
| 5 | 2 | 29.62 |
| 10 | 3 | 74.145 |

### Galileo's Insight

Galileo showed that the distance fallen from rest is proportional to $t^2$. Doubling the fall time quadruples the distance fallen. Mass does not appear — all objects fall at the same rate (ignoring air resistance).

### Your Task

Implement \`fallHeight(v0, t)\` returning the distance fallen in metres.`,

	starterCode: `#include <stdio.h>

#define G 9.81

double fallHeight(double v0, double t) {
    /* h = v0*t + 0.5*G*t*t */
    return 0;
}

int main() {
    printf("%.4f\\n", fallHeight(0, 1));   /* 4.9050  */
    printf("%.4f\\n", fallHeight(0, 2));   /* 19.6200 */
    printf("%.4f\\n", fallHeight(5, 2));   /* 29.6200 */
    printf("%.4f\\n", fallHeight(10, 3));  /* 74.1450 */
    return 0;
}
`,

	solution: `#include <stdio.h>

#define G 9.81

double fallHeight(double v0, double t) {
    return v0 * t + 0.5 * G * t * t;
}

int main() {
    printf("%.4f\\n", fallHeight(0, 1));   /* 4.9050  */
    printf("%.4f\\n", fallHeight(0, 2));   /* 19.6200 */
    printf("%.4f\\n", fallHeight(5, 2));   /* 29.6200 */
    printf("%.4f\\n", fallHeight(10, 3));  /* 74.1450 */
    return 0;
}
`,

	tests: [
		{
			name: "Dropped from rest, 1 s → 4.9050 m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", fallHeight(0, 1)); return 0; }`,
			expected: "4.9050\n",
		},
		{
			name: "Dropped from rest, 2 s → 19.6200 m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", fallHeight(0, 2)); return 0; }`,
			expected: "19.6200\n",
		},
		{
			name: "v0=5 m/s, 2 s → 29.6200 m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", fallHeight(5, 2)); return 0; }`,
			expected: "29.6200\n",
		},
		{
			name: "v0=10 m/s, 3 s → 74.1450 m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", fallHeight(10, 3)); return 0; }`,
			expected: "74.1450\n",
		},
	],
};
