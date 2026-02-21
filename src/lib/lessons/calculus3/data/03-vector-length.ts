import type { Lesson } from "../../types";

export const vectorLength3: Lesson = {
	id: "vec_length3",
	title: "Vector Magnitude",
	chapterId: "vectors-in-3d",
	content: `## Vector Magnitude in 3D

The **magnitude** (or length) of a vector $\\mathbf{v} = (x, y, z)$ is:

$$|\\mathbf{v}| = \\sqrt{x^2 + y^2 + z^2}$$

This is a direct extension of the Pythagorean theorem into three dimensions.

### Unit Vectors

A vector with magnitude 1 is called a **unit vector**. To normalize any vector:

$$\\hat{\\mathbf{v}} = \\frac{\\mathbf{v}}{|\\mathbf{v}|} = \\left(\\frac{x}{|\\mathbf{v}|},\\; \\frac{y}{|\\mathbf{v}|},\\; \\frac{z}{|\\mathbf{v}|}\\right)$$

Unit vectors indicate **direction only**.

### Distance Between Points

The distance between $P_1 = (x_1, y_1, z_1)$ and $P_2 = (x_2, y_2, z_2)$ is the magnitude of their difference:

$$d = \\sqrt{(x_2-x_1)^2 + (y_2-y_1)^2 + (z_2-z_1)^2}$$

### Examples

| Vector | Magnitude |
|--------|-----------|
| (3, 4, 0) | 5 |
| (1, 1, 1) | $\\sqrt{3} \\approx 1.7321$ |
| (2, 2, 1) | 3 |
| (0, 0, 0) | 0 |

### Your Task

Implement \`double vec_length3(double x, double y, double z)\` that returns the 3D vector magnitude.

Use \`#include <math.h>\` and the \`sqrt\` function.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double vec_length3(double x, double y, double z) {
\t/* return sqrt(x*x + y*y + z*z) */
\treturn 0.0;
}

int main() {
\t/* (3,4,0) has length 5 */
\tprintf("%.4f\\n", vec_length3(3, 4, 0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double vec_length3(double x, double y, double z) {
\treturn sqrt(x*x + y*y + z*z);
}

int main() {
\tprintf("%.4f\\n", vec_length3(3, 4, 0));
\treturn 0;
}
`,

	tests: [
		{
			name: "|(3,4,0)| = 5",
			expected: "5.0000\n",
		},
		{
			name: "|(2,2,1)| = 3",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", vec_length3(2, 2, 1));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "|(1,1,1)| = sqrt(3) ≈ 1.7321",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", vec_length3(1, 1, 1));
\treturn 0;
}`,
			expected: "1.7321\n",
		},
		{
			name: "|(0,0,0)| = 0",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", vec_length3(0, 0, 0));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
