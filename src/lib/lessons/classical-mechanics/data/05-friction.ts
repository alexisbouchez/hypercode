import type { Lesson } from "../../types";

export const frictionLesson: Lesson = {
	id: "friction",
	title: "Friction",
	chapterId: "forces",
	content: `## Friction

**Friction** is the contact force that opposes relative motion between surfaces. It is modelled as proportional to the normal force:

\`\`\`
f = μ × N
\`\`\`

- **μ** (mu) — coefficient of friction (dimensionless, depends on surface pair)
- **N** — normal force in Newtons (perpendicular to surface)
- **f** — friction force in Newtons

### Static vs Kinetic

| Type | Condition | Typical μ |
|------|-----------|-----------|
| Static (μₛ) | Object at rest | Higher |
| Kinetic (μₖ) | Object sliding | Lower |

For a block on a horizontal surface: N = mg, so f = μmg.

### Typical Coefficients

| Surface pair | μₖ |
|---|---|
| Rubber on dry concrete | 0.8 |
| Steel on steel | 0.57 |
| Ice on ice | 0.03 |
| Teflon on steel | 0.04 |

### Your Task

Implement \`frictionForce(mu, N)\` returning the friction force magnitude in Newtons.`,

	starterCode: `#include <stdio.h>

double frictionForce(double mu, double N) {
    /* f = mu * N */
    return 0;
}

int main() {
    printf("%.4f\\n", frictionForce(0.3, 100));  /* 30.0000 */
    printf("%.4f\\n", frictionForce(0.5, 50));   /* 25.0000 */
    printf("%.4f\\n", frictionForce(0.1, 200));  /* 20.0000 */
    printf("%.4f\\n", frictionForce(0.8, 75));   /* 60.0000 */
    return 0;
}
`,

	solution: `#include <stdio.h>

double frictionForce(double mu, double N) {
    return mu * N;
}

int main() {
    printf("%.4f\\n", frictionForce(0.3, 100));  /* 30.0000 */
    printf("%.4f\\n", frictionForce(0.5, 50));   /* 25.0000 */
    printf("%.4f\\n", frictionForce(0.1, 200));  /* 20.0000 */
    printf("%.4f\\n", frictionForce(0.8, 75));   /* 60.0000 */
    return 0;
}
`,

	tests: [
		{
			name: "μ=0.3, N=100 N → 30.0000 N",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", frictionForce(0.3, 100)); return 0; }`,
			expected: "30.0000\n",
		},
		{
			name: "μ=0.5, N=50 N → 25.0000 N",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", frictionForce(0.5, 50)); return 0; }`,
			expected: "25.0000\n",
		},
		{
			name: "μ=0.1, N=200 N → 20.0000 N",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", frictionForce(0.1, 200)); return 0; }`,
			expected: "20.0000\n",
		},
		{
			name: "μ=0.8, N=75 N → 60.0000 N",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", frictionForce(0.8, 75)); return 0; }`,
			expected: "60.0000\n",
		},
	],
};
