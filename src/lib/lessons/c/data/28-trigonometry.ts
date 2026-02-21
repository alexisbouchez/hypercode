import type { Lesson } from "../../types";

export const trigonometry: Lesson = {
	id: "trigonometry",
	title: "Trigonometry",
	chapterId: "mathematics",
	content: `## Trigonometric Functions in C

\`<math.h>\` provides the standard trig functions. All angles are in **radians**, not degrees.

### Functions

| Function | Description |
|----------|-------------|
| \`sin(x)\` | Sine of x (radians) |
| \`cos(x)\` | Cosine of x (radians) |
| \`tan(x)\` | Tangent of x (radians) |
| \`asin(x)\` | Inverse sine → angle in [-π/2, π/2] |
| \`acos(x)\` | Inverse cosine → angle in [0, π] |
| \`atan(x)\` | Inverse tangent → angle in (-π/2, π/2) |
| \`atan2(y, x)\` | Angle of vector (x,y) → full [-π, π] range |

### Degrees to Radians

To convert degrees to radians, multiply by π/180:

\`\`\`c
double deg = 90.0;
double rad = deg * M_PI / 180.0;
printf("%.4f\\n", sin(rad));  // 1.0000
\`\`\`

### Key Values to Know

\`\`\`
sin(0)     = 0       sin(π/6) = 0.5    sin(π/2) = 1
cos(0)     = 1       cos(π/3) = 0.5    cos(π/2) = 0
tan(π/4)   = 1
\`\`\`

### Using \`atan2\` for Angles

\`atan2(y, x)\` is the robust way to get the angle of a 2D vector — it handles all quadrants:

\`\`\`c
// Angle of the vector (1, 1) = 45° = π/4
double angle = atan2(1.0, 1.0);
printf("%.4f\\n", angle);          // 0.7854
printf("%.4f\\n", angle * 4.0);    // 3.1416 (π)
\`\`\`

### Distance Between Two Points

The Euclidean distance formula: $d = \\sqrt{(x_2-x_1)^2 + (y_2-y_1)^2}$

\`\`\`c
double dx = 3.0, dy = 4.0;
double dist = sqrt(dx*dx + dy*dy);  // 5.0 (classic 3-4-5 triangle)
\`\`\`

### Your Task

Using \`<math.h>\`, compute and print with \`%.4f\`:
1. \`sin(M_PI / 6.0)\` (sine of 30°)
2. \`cos(M_PI / 3.0)\` (cosine of 60°)
3. \`atan2(1.0, 1.0) * 4.0\` (reconstructing π)`,

	starterCode: `#include <stdio.h>
#include <math.h>

int main() {
\t// Print each result with %.4f
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

int main() {
\tprintf("%.4f\\n", sin(M_PI / 6.0));
\tprintf("%.4f\\n", cos(M_PI / 3.0));
\tprintf("%.4f\\n", atan2(1.0, 1.0) * 4.0);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints trigonometric results",
			expected: "0.5000\n0.5000\n3.1416\n",
		},
		{
			name: "sin(M_PI / 6.0) = 0.5000",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.4f\\n", sin(M_PI / 6.0));
\treturn 0;
}`,
			expected: "0.5000\n",
		},
		{
			name: "cos(M_PI / 3.0) = 0.5000",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.4f\\n", cos(M_PI / 3.0));
\treturn 0;
}`,
			expected: "0.5000\n",
		},
		{
			name: "atan2(1.0, 1.0) * 4.0 = pi",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.4f\\n", atan2(1.0, 1.0) * 4.0);
\treturn 0;
}`,
			expected: "3.1416\n",
		},
	],
};
