import type { Lesson } from "../../types";

export const centripetalLesson: Lesson = {
	id: "centripetal",
	title: "Centripetal Acceleration",
	chapterId: "forces",
	content: `## Circular Motion

An object moving in a circle at constant speed still **accelerates** — its direction changes continuously. This **centripetal acceleration** always points toward the centre:

\`\`\`
a_c = v² / r
\`\`\`

- **v** — speed (m/s)
- **r** — radius of circular path (m)
- **a_c** — centripetal acceleration (m/s²)

### Centripetal Force

By Newton's second law, the net force required to maintain circular motion:

\`\`\`
F_c = m × a_c = mv² / r
\`\`\`

This is not a new kind of force — it is whatever force provides the centripetal acceleration (tension in a string, gravity for orbits, friction for a car cornering).

### Examples

| v (m/s) | r (m) | a_c (m/s²) |
|---------|-------|------------|
| 10 | 5 | **20.0** |
| 20 | 4 | **100.0** |
| 3 | 9 | **1.0** |
| 15 | 10 | **22.5** |

### Your Task

Implement \`centripetal(v, r)\` returning centripetal acceleration in m/s².`,

	starterCode: `#include <stdio.h>

double centripetal(double v, double r) {
    /* a = v^2 / r */
    return 0;
}

int main() {
    printf("%.4f\\n", centripetal(10, 5));   /* 20.0000  */
    printf("%.4f\\n", centripetal(20, 4));   /* 100.0000 */
    printf("%.4f\\n", centripetal(3, 9));    /* 1.0000   */
    printf("%.4f\\n", centripetal(15, 10));  /* 22.5000  */
    return 0;
}
`,

	solution: `#include <stdio.h>

double centripetal(double v, double r) {
    return v * v / r;
}

int main() {
    printf("%.4f\\n", centripetal(10, 5));   /* 20.0000  */
    printf("%.4f\\n", centripetal(20, 4));   /* 100.0000 */
    printf("%.4f\\n", centripetal(3, 9));    /* 1.0000   */
    printf("%.4f\\n", centripetal(15, 10));  /* 22.5000  */
    return 0;
}
`,

	tests: [
		{
			name: "v=10 m/s, r=5 m → 20.0000 m/s²",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", centripetal(10, 5)); return 0; }`,
			expected: "20.0000\n",
		},
		{
			name: "v=20 m/s, r=4 m → 100.0000 m/s²",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", centripetal(20, 4)); return 0; }`,
			expected: "100.0000\n",
		},
		{
			name: "v=3 m/s, r=9 m → 1.0000 m/s²",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", centripetal(3, 9)); return 0; }`,
			expected: "1.0000\n",
		},
		{
			name: "v=15 m/s, r=10 m → 22.5000 m/s²",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", centripetal(15, 10)); return 0; }`,
			expected: "22.5000\n",
		},
	],
};
