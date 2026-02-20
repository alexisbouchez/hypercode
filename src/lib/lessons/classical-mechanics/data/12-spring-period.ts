import type { Lesson } from "../../types";

export const springPeriodLesson: Lesson = {
	id: "spring-period",
	title: "Spring-Mass Period",
	chapterId: "oscillations-and-gravitation",
	content: `## Period of a Spring-Mass System

A mass m on a spring with constant k oscillates with angular frequency:

\`\`\`
ω = √(k/m)
\`\`\`

The **period** (time for one complete oscillation):

\`\`\`
T = 2π/ω = 2π√(m/k)
\`\`\`

Units: seconds.

### Hooke's Law

The spring force: F = −kx. The spring constant k (N/m) measures stiffness — a stiffer spring (larger k) oscillates faster.

### Key Observations

- **Period increases with mass**: heavier objects oscillate more slowly (T ∝ √m)
- **Period decreases with stiffness**: stiffer springs oscillate faster (T ∝ 1/√k)
- **Period is independent of amplitude**: whether you stretch the spring 1 cm or 10 cm, the period is the same (for ideal springs)

### Examples

| m (kg) | k (N/m) | T (s) |
|--------|---------|-------|
| 1 | 1 | **6.2832** = 2π |
| 4 | 1 | **12.5664** = 4π |
| 1 | 4 | **3.1416** = π |
| 2 | 8 | **3.1416** = π |

### Your Task

Implement \`springPeriod(m, k)\` returning the oscillation period in seconds.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double springPeriod(double m, double k) {
    /* T = 2*PI * sqrt(m/k) */
    return 0;
}

int main() {
    printf("%.4f\\n", springPeriod(1, 1));   /* 6.2832  */
    printf("%.4f\\n", springPeriod(4, 1));   /* 12.5664 */
    printf("%.4f\\n", springPeriod(1, 4));   /* 3.1416  */
    printf("%.4f\\n", springPeriod(2, 8));   /* 3.1416  */
    return 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double springPeriod(double m, double k) {
    return 2.0 * PI * sqrt(m / k);
}

int main() {
    printf("%.4f\\n", springPeriod(1, 1));   /* 6.2832  */
    printf("%.4f\\n", springPeriod(4, 1));   /* 12.5664 */
    printf("%.4f\\n", springPeriod(1, 4));   /* 3.1416  */
    printf("%.4f\\n", springPeriod(2, 8));   /* 3.1416  */
    return 0;
}
`,

	tests: [
		{
			name: "m=1 kg, k=1 N/m → 6.2832 s (= 2π)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", springPeriod(1, 1)); return 0; }`,
			expected: "6.2832\n",
		},
		{
			name: "m=4 kg, k=1 N/m → 12.5664 s (= 4π)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", springPeriod(4, 1)); return 0; }`,
			expected: "12.5664\n",
		},
		{
			name: "m=1 kg, k=4 N/m → 3.1416 s (= π)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", springPeriod(1, 4)); return 0; }`,
			expected: "3.1416\n",
		},
		{
			name: "m=2 kg, k=8 N/m → 3.1416 s (= π)",
			code: `{{FUNC}}
int main() { printf("%.4f\\n", springPeriod(2, 8)); return 0; }`,
			expected: "3.1416\n",
		},
	],
};
