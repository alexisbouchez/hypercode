import type { Lesson } from "../../types";

export const mathFunctions: Lesson = {
	id: "math-functions",
	title: "Math Functions",
	chapterId: "mathematics",
	content: `## The \`<math.h>\` Library

C's standard math library provides a rich set of mathematical functions. Include it with:

\`\`\`c
#include <math.h>
\`\`\`

### Common Functions

| Function | Description | Example |
|----------|-------------|---------|
| \`sqrt(x)\` | Square root | \`sqrt(9.0)\` → 3.0 |
| \`pow(x, y)\` | x raised to power y | \`pow(2.0, 10.0)\` → 1024.0 |
| \`fabs(x)\` | Absolute value (float) | \`fabs(-3.5)\` → 3.5 |
| \`floor(x)\` | Round down | \`floor(4.9)\` → 4.0 |
| \`ceil(x)\` | Round up | \`ceil(4.1)\` → 5.0 |
| \`round(x)\` | Round to nearest | \`round(3.5)\` → 4.0 |

> All math functions work on \`double\` (64-bit floating-point). Use \`%f\` or \`%.Nf\` to print them.

### Printing Floats

Use \`%f\` for default 6 decimal places, or \`%.Nf\` for exactly N:

\`\`\`c
printf("%f\\n", sqrt(2.0));    // 1.414214
printf("%.2f\\n", sqrt(2.0));  // 1.41
printf("%.4f\\n", sqrt(2.0));  // 1.4142
\`\`\`

### Integer vs Floating-Point Absolute Value

Note: \`abs()\` from \`<stdlib.h>\` is for integers. Use \`fabs()\` for doubles:

\`\`\`c
int a = abs(-5);       // 5  (integer)
double b = fabs(-5.0); // 5.0 (floating-point)
\`\`\`

### Your Task

Using \`<math.h>\`, compute and print with \`%.2f\`:
1. \`sqrt(144.0)\`
2. \`pow(2.0, 8.0)\`
3. \`fabs(-3.14)\`
4. \`floor(4.9)\`
5. \`ceil(4.1)\`
6. \`round(3.5)\``,

	starterCode: `#include <stdio.h>
#include <math.h>

int main() {
\t// Print each result on its own line with %.2f
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

int main() {
\tprintf("%.2f\\n", sqrt(144.0));
\tprintf("%.2f\\n", pow(2.0, 8.0));
\tprintf("%.2f\\n", fabs(-3.14));
\tprintf("%.2f\\n", floor(4.9));
\tprintf("%.2f\\n", ceil(4.1));
\tprintf("%.2f\\n", round(3.5));
\treturn 0;
}
`,

	tests: [
		{
			name: "prints math function results",
			expected: "12.00\n256.00\n3.14\n4.00\n5.00\n4.00\n",
		},
		{
			name: "sqrt(144.0) = 12.00",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.2f\\n", sqrt(144.0));
\treturn 0;
}`,
			expected: "12.00\n",
		},
		{
			name: "pow(2.0, 8.0) = 256.00",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.2f\\n", pow(2.0, 8.0));
\treturn 0;
}`,
			expected: "256.00\n",
		},
		{
			name: "fabs(-3.14) = 3.14",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.2f\\n", fabs(-3.14));
\treturn 0;
}`,
			expected: "3.14\n",
		},
	],
};
