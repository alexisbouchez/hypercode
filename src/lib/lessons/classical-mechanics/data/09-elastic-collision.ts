import type { Lesson } from "../../types";

export const elasticCollisionLesson: Lesson = {
	id: "elastic-collision",
	title: "Elastic Collision",
	chapterId: "energy-and-momentum",
	content: `## Conservation of Momentum

In any collision, **total momentum is conserved**:

\`\`\`
m₁v₁ + m₂v₂ = m₁v₁' + m₂v₂'
\`\`\`

### Elastic Collisions

An **elastic** collision also conserves kinetic energy. Solving both conservation equations simultaneously gives the final velocities:

\`\`\`
v₁' = ((m₁ − m₂)v₁ + 2m₂v₂) / (m₁ + m₂)
v₂' = ((m₂ − m₁)v₂ + 2m₁v₁) / (m₁ + m₂)
\`\`\`

### Special Cases

| Scenario | Result |
|----------|--------|
| Equal masses (v₂=0) | v₁'=0, v₂'=v₁ (complete transfer) |
| Heavy hits light (m₁≫m₂) | v₁'≈v₁, v₂'≈2v₁ (light bounces fast) |
| Light hits heavy (m₁≪m₂) | v₁'≈−v₁, v₂'≈0 (light bounces back) |

### Examples

| m₁ | v₁ | m₂ | v₂ | v₁' |
|----|----|----|----|----|
| 1 | 10 | 1 | 0 | 0.0000 (stops) |
| 2 | 10 | 1 | 0 | 3.3333 |
| 1 | 0 | 2 | 5 | 6.6667 |

### Your Task

Implement \`elasticV1(m1, v1, m2, v2)\` returning the post-collision velocity of mass 1.`,

	starterCode: `#include <stdio.h>

double elasticV1(double m1, double v1, double m2, double v2) {
    /* v1' = ((m1 - m2)*v1 + 2*m2*v2) / (m1 + m2) */
    return 0;
}

int main() {
    printf("%.4f\\n", elasticV1(1, 10, 1, 0));   /* 0.0000  (equal mass, full transfer) */
    printf("%.4f\\n", elasticV1(2, 10, 1, 0));   /* 3.3333  */
    printf("%.4f\\n", elasticV1(1, 0, 2, 5));    /* 6.6667  */
    printf("%.4f\\n", elasticV1(3, 10, 3, 0));   /* 0.0000  (equal mass) */
    return 0;
}
`,

	solution: `#include <stdio.h>

double elasticV1(double m1, double v1, double m2, double v2) {
    return ((m1 - m2) * v1 + 2.0 * m2 * v2) / (m1 + m2);
}

int main() {
    printf("%.4f\\n", elasticV1(1, 10, 1, 0));   /* 0.0000  */
    printf("%.4f\\n", elasticV1(2, 10, 1, 0));   /* 3.3333  */
    printf("%.4f\\n", elasticV1(1, 0, 2, 5));    /* 6.6667  */
    printf("%.4f\\n", elasticV1(3, 10, 3, 0));   /* 0.0000  */
    return 0;
}
`,

	tests: [
		{
			name: "Equal masses, 1 hits stationary → 0.0000 (full stop)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", elasticV1(1, 10, 1, 0)); return 0; }`,
			expected: "0.0000\n",
		},
		{
			name: "2 kg hits 1 kg → 3.3333 m/s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", elasticV1(2, 10, 1, 0)); return 0; }`,
			expected: "3.3333\n",
		},
		{
			name: "1 kg (rest) hit by 2 kg at 5 m/s → 6.6667 m/s",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", elasticV1(1, 0, 2, 5)); return 0; }`,
			expected: "6.6667\n",
		},
		{
			name: "Equal masses 3 kg, v2=0 → 0.0000 (full stop)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", elasticV1(3, 10, 3, 0)); return 0; }`,
			expected: "0.0000\n",
		},
	],
};
