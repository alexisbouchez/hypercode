import type { Lesson } from "../../types";

export const accelerationLesson: Lesson = {
	id: "acceleration",
	title: "Newton's Second Law",
	chapterId: "forces",
	content: `## Newton's Second Law

The most important equation in classical mechanics:

\`\`\`
F = ma    →    a = F / m
\`\`\`

**Force** (F) is measured in Newtons (N = kg·m/s²).
**Mass** (m) in kilograms.
**Acceleration** (a) in m/s².

### Physical Meaning

A net force causes acceleration proportional to the force and inversely proportional to mass. A heavier object requires more force to achieve the same acceleration.

### Examples

| Force (N) | Mass (kg) | Acceleration (m/s²) |
|-----------|-----------|---------------------|
| 100 | 10 | **10.0** |
| 50 | 2 | **25.0** |
| 9.81 | 1 | **9.81** ← gravitational acceleration |
| 200 | 8 | **25.0** |

### Net Force

The F in F=ma is always the **net** force — the vector sum of all forces acting on the object. If you push a block with 50 N and friction provides 20 N opposing, the net force is 30 N.

### Your Task

Implement \`acceleration(F, m)\` returning acceleration in m/s².`,

	starterCode: `#include <stdio.h>

double acceleration(double F, double m) {
    /* a = F / m */
    return 0;
}

int main() {
    printf("%.4f\\n", acceleration(100, 10));  /* 10.0000 */
    printf("%.4f\\n", acceleration(50, 2));    /* 25.0000 */
    printf("%.4f\\n", acceleration(9.81, 1));  /* 9.8100  */
    printf("%.4f\\n", acceleration(200, 8));   /* 25.0000 */
    return 0;
}
`,

	solution: `#include <stdio.h>

double acceleration(double F, double m) {
    return F / m;
}

int main() {
    printf("%.4f\\n", acceleration(100, 10));  /* 10.0000 */
    printf("%.4f\\n", acceleration(50, 2));    /* 25.0000 */
    printf("%.4f\\n", acceleration(9.81, 1));  /* 9.8100  */
    printf("%.4f\\n", acceleration(200, 8));   /* 25.0000 */
    return 0;
}
`,

	tests: [
		{
			name: "100 N on 10 kg → 10.0000 m/s²",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", acceleration(100, 10)); return 0; }`,
			expected: "10.0000\n",
		},
		{
			name: "50 N on 2 kg → 25.0000 m/s²",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", acceleration(50, 2)); return 0; }`,
			expected: "25.0000\n",
		},
		{
			name: "9.81 N on 1 kg → 9.8100 m/s² (gravity)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", acceleration(9.81, 1)); return 0; }`,
			expected: "9.8100\n",
		},
		{
			name: "200 N on 8 kg → 25.0000 m/s²",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", acceleration(200, 8)); return 0; }`,
			expected: "25.0000\n",
		},
	],
};
