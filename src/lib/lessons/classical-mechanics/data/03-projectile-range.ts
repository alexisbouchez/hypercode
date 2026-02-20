import type { Lesson } from "../../types";

export const projectileRangeLesson: Lesson = {
	id: "projectile-range",
	title: "Projectile Range",
	chapterId: "kinematics",
	content: `## Projectile Motion

When an object is launched at speed v₀ and angle θ above horizontal (ignoring air resistance), it follows a parabolic path. Horizontal and vertical motion are **independent**.

### Horizontal Range

The total horizontal distance (range) when landing at the same height:

\`\`\`
R = v₀² × sin(2θ) / g
\`\`\`

### Maximum Height

\`\`\`
h_max = v₀² × sin²(θ) / (2g)
\`\`\`

### Key Observations

- **45° gives maximum range**: sin(2 × 45°) = sin(90°) = 1
- **Complementary angles have equal range**: 30° and 60° give the same R
- Range scales as v₀² — doubling launch speed quadruples range

### Examples at v₀ = 20 m/s

| θ | R (m) |
|---|-------|
| 30° | 35.3119 |
| 45° | 40.7747 |
| 60° | 35.3119 |

### Your Task

Implement \`projectileRange(v0, angle_deg)\` returning range in metres.

**Hint:** Convert degrees to radians: \`angle_rad = angle_deg × π / 180\`.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define G    9.81
#define PI   3.14159265358979

double projectileRange(double v0, double angle_deg) {
    /* R = v0^2 * sin(2*theta) / g  (theta in radians) */
    return 0;
}

int main() {
    printf("%.4f\\n", projectileRange(10, 45));  /* 10.1937 */
    printf("%.4f\\n", projectileRange(20, 45));  /* 40.7747 */
    printf("%.4f\\n", projectileRange(20, 30));  /* 35.3119 */
    printf("%.4f\\n", projectileRange(30, 45));  /* 91.7431 */
    return 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define G    9.81
#define PI   3.14159265358979

double projectileRange(double v0, double angle_deg) {
    double angle = angle_deg * PI / 180.0;
    return v0 * v0 * sin(2.0 * angle) / G;
}

int main() {
    printf("%.4f\\n", projectileRange(10, 45));  /* 10.1937 */
    printf("%.4f\\n", projectileRange(20, 45));  /* 40.7747 */
    printf("%.4f\\n", projectileRange(20, 30));  /* 35.3119 */
    printf("%.4f\\n", projectileRange(30, 45));  /* 91.7431 */
    return 0;
}
`,

	tests: [
		{
			name: "10 m/s at 45° → 10.1937 m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", projectileRange(10, 45)); return 0; }`,
			expected: "10.1937\n",
		},
		{
			name: "20 m/s at 45° → 40.7747 m (maximum range)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", projectileRange(20, 45)); return 0; }`,
			expected: "40.7747\n",
		},
		{
			name: "20 m/s at 30° → 35.3119 m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", projectileRange(20, 30)); return 0; }`,
			expected: "35.3119\n",
		},
		{
			name: "30 m/s at 45° → 91.7431 m",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", projectileRange(30, 45)); return 0; }`,
			expected: "91.7431\n",
		},
	],
};
