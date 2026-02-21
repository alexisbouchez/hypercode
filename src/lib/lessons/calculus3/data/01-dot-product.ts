import type { Lesson } from "../../types";

export const dotProduct3: Lesson = {
	id: "dot3",
	title: "3D Dot Product",
	chapterId: "vectors-in-3d",
	content: `## The Dot Product in 3D

The **dot product** of two vectors $\\mathbf{a} = (a_x, a_y, a_z)$ and $\\mathbf{b} = (b_x, b_y, b_z)$ is:

$$\\mathbf{a} \\cdot \\mathbf{b} = a_x b_x + a_y b_y + a_z b_z$$

The result is a **scalar** — a single number, not a vector.

### Geometric Interpretation

$$\\mathbf{a} \\cdot \\mathbf{b} = |\\mathbf{a}| \\cdot |\\mathbf{b}| \\cdot \\cos(\\theta)$$

Where $\\theta$ is the angle between the vectors. This means:
- If $\\mathbf{a} \\cdot \\mathbf{b} > 0$: vectors point in a similar direction ($\\theta < 90°$)
- If $\\mathbf{a} \\cdot \\mathbf{b} = 0$: vectors are **perpendicular** ($\\theta = 90°$)
- If $\\mathbf{a} \\cdot \\mathbf{b} < 0$: vectors point in opposite directions ($\\theta > 90°$)

### Applications

- **Checking orthogonality**: two vectors are perpendicular if and only if their dot product is zero
- **Work**: $W = \\mathbf{F} \\cdot \\mathbf{d}$ (force dot displacement)
- **Projections**: computing how much one vector points along another

### Examples

| $\\mathbf{a}$ | $\\mathbf{b}$ | $\\mathbf{a} \\cdot \\mathbf{b}$ |
|-------|-------|-----------|
| (1,0,0) | (0,1,0) | 0 (perpendicular) |
| (1,2,3) | (4,5,6) | 4+10+18 = 32 |
| (2,2,1) | (2,2,1) | $4+4+1 = 9 = |\\mathbf{a}|^2$ |

### Your Task

Implement \`double dot3(double ax, double ay, double az, double bx, double by, double bz)\` that computes the 3D dot product.`,

	starterCode: `#include <stdio.h>

double dot3(double ax, double ay, double az,
            double bx, double by, double bz) {
\t/* return ax*bx + ay*by + az*bz */
\treturn 0.0;
}

int main() {
\t/* (1,2,3) · (4,5,6) = 4+10+18 = 32 */
\tprintf("%.4f\\n", dot3(1,2,3, 4,5,6));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double dot3(double ax, double ay, double az,
            double bx, double by, double bz) {
\treturn ax*bx + ay*by + az*bz;
}

int main() {
\tprintf("%.4f\\n", dot3(1,2,3, 4,5,6));
\treturn 0;
}
`,

	tests: [
		{
			name: "(1,2,3)·(4,5,6) = 32",
			expected: "32.0000\n",
		},
		{
			name: "perpendicular vectors: (1,0,0)·(0,1,0) = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", dot3(1,0,0, 0,1,0));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "(2,2,1)·(2,2,1) = 9",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", dot3(2,2,1, 2,2,1));
\treturn 0;
}`,
			expected: "9.0000\n",
		},
		{
			name: "(1,0,0)·(1,1,1) = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", dot3(1,0,0, 1,1,1));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
	],
};
