import type { Lesson } from "../../types";

export const crossProduct3: Lesson = {
	id: "cross3",
	title: "3D Cross Product",
	chapterId: "vectors-in-3d",
	content: `## The Cross Product

The **cross product** of $\\mathbf{a} = (a_x, a_y, a_z)$ and $\\mathbf{b} = (b_x, b_y, b_z)$ produces a **new vector** perpendicular to both:

$$\\mathbf{a} \\times \\mathbf{b} = (a_y b_z - a_z b_y,\\; a_z b_x - a_x b_z,\\; a_x b_y - a_y b_x)$$

### Key Properties

- **Direction**: perpendicular to both $\\mathbf{a}$ and $\\mathbf{b}$ (right-hand rule)
- **Magnitude**: $|\\mathbf{a} \\times \\mathbf{b}| = |\\mathbf{a}| \\cdot |\\mathbf{b}| \\cdot \\sin(\\theta)$
- **Anti-commutative**: $\\mathbf{a} \\times \\mathbf{b} = -(\\mathbf{b} \\times \\mathbf{a})$
- If $\\mathbf{a}$ and $\\mathbf{b}$ are parallel: $\\mathbf{a} \\times \\mathbf{b} = \\mathbf{0}$

### Geometric Meaning

The magnitude $|\\mathbf{a} \\times \\mathbf{b}|$ equals the **area of the parallelogram** spanned by $\\mathbf{a}$ and $\\mathbf{b}$.

### Memory Aid: The Determinant Formula

$$\\mathbf{a} \\times \\mathbf{b} = \\begin{vmatrix} \\mathbf{i} & \\mathbf{j} & \\mathbf{k} \\\\ a_x & a_y & a_z \\\\ b_x & b_y & b_z \\end{vmatrix}$$

Expanding along the first row gives the formula above.

### Applications

- **Normal vectors** to planes and surfaces
- **Torque**: $\\boldsymbol{\\tau} = \\mathbf{r} \\times \\mathbf{F}$
- **Area** of triangles and parallelograms in 3D

### Your Task

Implement \`void cross3(double ax, double ay, double az, double bx, double by, double bz, double *rx, double *ry, double *rz)\` that writes the cross product into the output pointers.`,

	starterCode: `#include <stdio.h>

void cross3(double ax, double ay, double az,
            double bx, double by, double bz,
            double *rx, double *ry, double *rz) {
\t/* compute a x b and store in *rx, *ry, *rz */
}

int main() {
\tdouble rx, ry, rz;
\t/* (1,0,0) x (0,1,0) = (0,0,1) */
\tcross3(1,0,0, 0,1,0, &rx, &ry, &rz);
\tprintf("%.4f %.4f %.4f\\n", rx, ry, rz);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void cross3(double ax, double ay, double az,
            double bx, double by, double bz,
            double *rx, double *ry, double *rz) {
\t*rx = ay*bz - az*by;
\t*ry = az*bx - ax*bz;
\t*rz = ax*by - ay*bx;
}

int main() {
\tdouble rx, ry, rz;
\tcross3(1,0,0, 0,1,0, &rx, &ry, &rz);
\tprintf("%.4f %.4f %.4f\\n", rx, ry, rz);
\treturn 0;
}
`,

	tests: [
		{
			name: "(1,0,0)×(0,1,0) = (0,0,1)",
			expected: "0.0000 0.0000 1.0000\n",
		},
		{
			name: "(1,2,3)×(4,5,6) = (-3,6,-3)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble rx, ry, rz;
\tcross3(1,2,3, 4,5,6, &rx, &ry, &rz);
\tprintf("%.4f %.4f %.4f\\n", rx, ry, rz);
\treturn 0;
}`,
			expected: "-3.0000 6.0000 -3.0000\n",
		},
		{
			name: "parallel vectors: (1,0,0)×(2,0,0) = (0,0,0)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble rx, ry, rz;
\tcross3(1,0,0, 2,0,0, &rx, &ry, &rz);
\tprintf("%.4f %.4f %.4f\\n", rx, ry, rz);
\treturn 0;
}`,
			expected: "0.0000 0.0000 0.0000\n",
		},
		{
			name: "(0,1,0)×(0,0,1) = (1,0,0)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble rx, ry, rz;
\tcross3(0,1,0, 0,0,1, &rx, &ry, &rz);
\tprintf("%.4f %.4f %.4f\\n", rx, ry, rz);
\treturn 0;
}`,
			expected: "1.0000 0.0000 0.0000\n",
		},
	],
};
