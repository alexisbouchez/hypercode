import type { Lesson } from "../../types";

export const powerLesson: Lesson = {
	id: "power",
	title: "Power",
	chapterId: "energy-and-momentum",
	content: `## Power

**Power** is the rate at which work is done — energy transferred per unit time:

\`\`\`
P = W / t
\`\`\`

Units: Watts (W = J/s). One watt is one joule delivered per second.

### Alternative Forms

Since W = F × d and v = d/t:

\`\`\`
P = F × v
\`\`\`

Power equals force times velocity — useful for engines and motors.

### Common Powers

| Source | Power |
|--------|-------|
| Human at rest (basal) | ~80 W |
| Cyclist (racing) | ~400 W |
| Car engine | ~100,000 W (100 kW) |
| Large wind turbine | ~5,000,000 W (5 MW) |

### Horsepower

1 horsepower (hp) ≈ 746 W. A 100 hp engine ≈ 74.6 kW.

### Examples

| Work (J) | Time (s) | Power (W) |
|----------|----------|-----------|
| 1000 | 10 | **100.0** |
| 500 | 5 | **100.0** |
| 3600 | 3600 | **1.0** (1 Wh in 1 h) |
| 7500 | 1 | **7500.0** |

### Your Task

Implement \`power(W, t)\` returning power in Watts.`,

	starterCode: `#include <stdio.h>

double power(double W, double t) {
    /* P = W / t */
    return 0;
}

int main() {
    printf("%.4f\\n", power(1000, 10));    /* 100.0000  */
    printf("%.4f\\n", power(500, 5));      /* 100.0000  */
    printf("%.4f\\n", power(3600, 3600));  /* 1.0000    */
    printf("%.4f\\n", power(7500, 1));     /* 7500.0000 */
    return 0;
}
`,

	solution: `#include <stdio.h>

double power(double W, double t) {
    return W / t;
}

int main() {
    printf("%.4f\\n", power(1000, 10));    /* 100.0000  */
    printf("%.4f\\n", power(500, 5));      /* 100.0000  */
    printf("%.4f\\n", power(3600, 3600));  /* 1.0000    */
    printf("%.4f\\n", power(7500, 1));     /* 7500.0000 */
    return 0;
}
`,

	tests: [
		{
			name: "1000 J in 10 s → 100.0000 W",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", power(1000, 10)); return 0; }`,
			expected: "100.0000\n",
		},
		{
			name: "500 J in 5 s → 100.0000 W",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", power(500, 5)); return 0; }`,
			expected: "100.0000\n",
		},
		{
			name: "3600 J in 3600 s → 1.0000 W",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", power(3600, 3600)); return 0; }`,
			expected: "1.0000\n",
		},
		{
			name: "7500 J in 1 s → 7500.0000 W",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", power(7500, 1)); return 0; }`,
			expected: "7500.0000\n",
		},
	],
};
