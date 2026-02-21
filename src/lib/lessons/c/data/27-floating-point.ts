import type { Lesson } from "../../types";

export const floatingPoint: Lesson = {
	id: "floating-point",
	title: "Floating-Point Numbers",
	chapterId: "mathematics",
	content: `## Floating-Point in C

C has two floating-point types:

| Type | Size | Precision | Suffix |
|------|------|-----------|--------|
| \`float\` | 32-bit | ~7 decimal digits | \`f\` literal: \`3.14f\` |
| \`double\` | 64-bit | ~15 decimal digits | default: \`3.14\` |

> Always prefer \`double\` for mathematical computation — it has far more precision and all \`<math.h>\` functions operate on it.

### Floating-Point Division

Unlike integers, dividing two doubles gives a real result:

\`\`\`c
double a = 1.0 / 3.0;
printf("%.6f\\n", a);  // 0.333333
\`\`\`

### Constants in \`<math.h>\`

\`<math.h>\` defines useful constants:

\`\`\`c
M_PI     // π ≈ 3.14159265358979
M_E      // e ≈ 2.71828182845905
M_SQRT2  // √2 ≈ 1.41421356237310
\`\`\`

### Comparing Doubles

**Never** use \`==\` to compare floating-point numbers — rounding errors accumulate:

\`\`\`c
double a = 0.1 + 0.2;
// a is 0.30000000000000004, not 0.3!
// Use a small epsilon instead:
double epsilon = 1e-9;
if (fabs(a - 0.3) < epsilon) {
    printf("equal\\n");
}
\`\`\`

### Computing with Geometry

\`\`\`c
double r = 5.0;
double area = M_PI * r * r;  // 78.5398...
printf("%.4f\\n", area);      // 78.5398
\`\`\`

### Your Task

Using \`double\` and \`<math.h>\`, compute and print:
1. \`1.0 / 3.0\` with \`%.6f\`
2. The area of a circle with radius 5.0 (\`M_PI * r * r\`) with \`%.4f\`
3. \`sqrt(2.0)\` with \`%.6f\``,

	starterCode: `#include <stdio.h>
#include <math.h>

int main() {
\t// 1. Print 1.0/3.0 with %.6f
\t// 2. Print area of circle r=5 with %.4f
\t// 3. Print sqrt(2.0) with %.6f
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

int main() {
\tprintf("%.6f\\n", 1.0 / 3.0);
\tdouble r = 5.0;
\tprintf("%.4f\\n", M_PI * r * r);
\tprintf("%.6f\\n", sqrt(2.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "prints floating-point results",
			expected: "0.333333\n78.5398\n1.414214\n",
		},
		{
			name: "1.0 / 3.0 printed with %.6f",
			code: `#include <stdio.h>
int main() {
\tprintf("%.6f\\n", 1.0 / 3.0);
\treturn 0;
}`,
			expected: "0.333333\n",
		},
		{
			name: "circle area r=5 with %.4f",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tdouble r = 5.0;
\tprintf("%.4f\\n", M_PI * r * r);
\treturn 0;
}`,
			expected: "78.5398\n",
		},
	],
};
