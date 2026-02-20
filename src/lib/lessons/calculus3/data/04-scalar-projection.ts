import type { Lesson } from "../../types";

export const scalarProjection: Lesson = {
	id: "scalar_proj",
	title: "Scalar Projection",
	chapterId: "vectors-in-3d",
	content: `## Scalar Projection

The **scalar projection** of **a** onto **b** answers: "how much of **a** points in the direction of **b**?"

\`\`\`
comp_b(a) = (a · b) / |b|
\`\`\`

This is the signed length of the shadow of **a** onto the line through **b**.

### Derivation

From the dot product formula: **a · b** = |**a**| · |**b**| · cos(θ)

Dividing by |**b**|:

\`\`\`
comp_b(a) = |a| cos(θ) = (a · b) / |b|
\`\`\`

### Vector Projection

The **vector projection** (not required here) returns the actual vector:

\`\`\`
proj_b(a) = ((a · b) / |b|²) · b
\`\`\`

### Examples

| **a** | **b** | comp_b(**a**) |
|-------|-------|---------------|
| (3,4,0) | (1,0,0) | 3 (just the x-component) |
| (1,1,1) | (0,1,0) | 1 (just the y-component) |
| (3,4,0) | (3,4,0) | 5 = |**a**| (projecting onto itself) |

### Your Task

Implement \`double scalar_proj(double ax, double ay, double az, double bx, double by, double bz)\` that returns the scalar projection of **a** onto **b**.

Use \`#include <math.h>\` for \`sqrt\`.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double scalar_proj(double ax, double ay, double az,
                   double bx, double by, double bz) {
\t/* return (a dot b) / |b| */
\treturn 0.0;
}

int main() {
\t/* projection of (3,4,0) onto (1,0,0) = 3 */
\tprintf("%.4f\\n", scalar_proj(3,4,0, 1,0,0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double scalar_proj(double ax, double ay, double az,
                   double bx, double by, double bz) {
\tdouble dot = ax*bx + ay*by + az*bz;
\tdouble len_b = sqrt(bx*bx + by*by + bz*bz);
\treturn dot / len_b;
}

int main() {
\tprintf("%.4f\\n", scalar_proj(3,4,0, 1,0,0));
\treturn 0;
}
`,

	tests: [
		{
			name: "comp of (3,4,0) onto (1,0,0) = 3",
			expected: "3.0000\n",
		},
		{
			name: "comp of (1,1,1) onto (0,1,0) = 1",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", scalar_proj(1,1,1, 0,1,0));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "comp of (3,4,0) onto itself = 5",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", scalar_proj(3,4,0, 3,4,0));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
		{
			name: "comp of (2,0,0) onto (3,4,0) = 6/5 = 1.2",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", scalar_proj(2,0,0, 3,4,0));
\treturn 0;
}`,
			expected: "1.2000\n",
		},
	],
};
