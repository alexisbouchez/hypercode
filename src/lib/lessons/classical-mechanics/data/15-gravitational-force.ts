import type { Lesson } from "../../types";

export const gravitationalForceLesson: Lesson = {
	id: "gravitational-force",
	title: "Newton's Law of Gravitation",
	chapterId: "oscillations-and-gravitation",
	content: `## Universal Gravitation

Every mass attracts every other mass. Newton's law of universal gravitation:

\`\`\`
F = G × m₁ × m₂ / r²
\`\`\`

- **G** = 6.674 × 10⁻¹¹ N·m²/kg² — the gravitational constant
- **m₁, m₂** — masses in kg
- **r** — distance between centres in metres
- **F** — attractive force in Newtons

### The Inverse-Square Law

Force falls off as 1/r². Double the distance → one quarter the force. This is why the Moon orbits Earth rather than flying away — gravity weakens with distance, but never reaches zero.

### Relation to g = 9.81 m/s²

At Earth's surface (r = 6.371 × 10⁶ m, M_Earth = 5.972 × 10²⁴ kg):

\`\`\`
g = G × M_Earth / r² ≈ 9.81 m/s²
\`\`\`

The familiar constant g is just Newton's law applied at Earth's surface.

### Examples (using m₁ = m₂ = 10¹⁰ kg)

| r (m) | F (N) |
|-------|-------|
| 1 | **6.6740** |
| 2 | **1.6685** |
| 10 | **0.0667** |

### Your Task

Implement \`gravForce(m1, m2, r)\` returning the gravitational force in Newtons.`,

	starterCode: `#include <stdio.h>

#define G_CONST 6.674e-11

double gravForce(double m1, double m2, double r) {
    /* F = G * m1 * m2 / r^2 */
    return 0;
}

int main() {
    printf("%.4f\\n", gravForce(1e10, 1e10, 1));   /* 6.6740 */
    printf("%.4f\\n", gravForce(1e10, 1e10, 2));   /* 1.6685 */
    printf("%.4f\\n", gravForce(2e10, 1e10, 1));   /* 13.3480 */
    printf("%.4f\\n", gravForce(1e10, 1e10, 10));  /* 0.0667 */
    return 0;
}
`,

	solution: `#include <stdio.h>

#define G_CONST 6.674e-11

double gravForce(double m1, double m2, double r) {
    return G_CONST * m1 * m2 / (r * r);
}

int main() {
    printf("%.4f\\n", gravForce(1e10, 1e10, 1));   /* 6.6740 */
    printf("%.4f\\n", gravForce(1e10, 1e10, 2));   /* 1.6685 */
    printf("%.4f\\n", gravForce(2e10, 1e10, 1));   /* 13.3480 */
    printf("%.4f\\n", gravForce(1e10, 1e10, 10));  /* 0.0667 */
    return 0;
}
`,

	tests: [
		{
			name: "10¹⁰ kg × 10¹⁰ kg at 1 m → 6.6740 N",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravForce(1e10, 1e10, 1)); return 0; }`,
			expected: "6.6740\n",
		},
		{
			name: "10¹⁰ kg × 10¹⁰ kg at 2 m → 1.6685 N (¼ force)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravForce(1e10, 1e10, 2)); return 0; }`,
			expected: "1.6685\n",
		},
		{
			name: "2×10¹⁰ kg × 10¹⁰ kg at 1 m → 13.3480 N",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravForce(2e10, 1e10, 1)); return 0; }`,
			expected: "13.3480\n",
		},
		{
			name: "10¹⁰ kg × 10¹⁰ kg at 10 m → 0.0667 N",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", gravForce(1e10, 1e10, 10)); return 0; }`,
			expected: "0.0667\n",
		},
	],
};
