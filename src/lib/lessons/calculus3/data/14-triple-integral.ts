import type { Lesson } from "../../types";

export const tripleIntegral: Lesson = {
	id: "triple_integral",
	title: "Triple Integral",
	chapterId: "multiple-integrals",
	content: `## Triple Integrals

A **triple integral** integrates over a 3D box [xa,xb] × [ya,yb] × [za,zb]:

\`\`\`
∭_V f(x,y,z) dV
\`\`\`

### The 3D Midpoint Rule

Divide each axis into n equal intervals. Evaluate f at each cell center:

\`\`\`
∭f dV ≈ Σ_i Σ_j Σ_k f(x_i, y_j, z_k) · Δx · Δy · Δz
\`\`\`

### Geometric Meaning

- **∭ 1 dV** = volume of the region
- **∭ ρ(x,y,z) dV** = total mass, if ρ is density
- **∭ f dV / Volume** = average value of f over the region

### Examples

**∭ 1 dV over [0,2]×[0,3]×[0,4]** = 24 (volume of the box)

**∭ x dV over [0,1]³** = ½ · 1 · 1 = ½

**∭ (x+y+z) dV over [0,1]³** = 3/2 (by symmetry and linearity)

### Applications

- Volume calculations of 3D regions
- Mass of non-uniform solid objects
- Center of mass calculations
- Moment of inertia

### Your Task

Implement \`double triple_integral(double (*f)(double, double, double), double xa, double xb, double ya, double yb, double za, double zb, int n)\` using the 3D midpoint rule with n divisions per axis.`,

	starterCode: `#include <stdio.h>

double triple_integral(double (*f)(double, double, double),
                        double xa, double xb,
                        double ya, double yb,
                        double za, double zb,
                        int n) {
\t/* 3D midpoint rule with n steps per axis */
\treturn 0.0;
}

double one(double x, double y, double z) { return 1.0; }

int main() {
\t/* volume of [0,2]x[0,3]x[0,4] = 24 */
\tprintf("%.4f\\n", triple_integral(one, 0,2, 0,3, 0,4, 20));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double triple_integral(double (*f)(double, double, double),
                        double xa, double xb,
                        double ya, double yb,
                        double za, double zb,
                        int n) {
\tdouble dx = (xb - xa) / n;
\tdouble dy = (yb - ya) / n;
\tdouble dz = (zb - za) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = xa + (i + 0.5) * dx;
\t\tfor (int j = 0; j < n; j++) {
\t\t\tdouble y = ya + (j + 0.5) * dy;
\t\t\tfor (int k = 0; k < n; k++) {
\t\t\t\tdouble z = za + (k + 0.5) * dz;
\t\t\t\tsum += f(x, y, z);
\t\t\t}
\t\t}
\t}
\treturn sum * dx * dy * dz;
}

double one(double x, double y, double z) { return 1.0; }

int main() {
\tprintf("%.4f\\n", triple_integral(one, 0,2, 0,3, 0,4, 20));
\treturn 0;
}
`,

	tests: [
		{
			name: "∭1 dV over [0,2]×[0,3]×[0,4] = 24",
			expected: "24.0000\n",
		},
		{
			name: "∭1 dV over [0,1]³ = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", triple_integral(one, 0,1, 0,1, 0,1, 20));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "∭x dV over [0,1]³ = 0.5",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y, double z) { return x + y*0 + z*0; }
int main() {
\tprintf("%.4f\\n", triple_integral(g, 0,1, 0,1, 0,1, 20));
\treturn 0;
}`,
			expected: "0.5000\n",
		},
		{
			name: "∭(x+y+z) dV over [0,1]³ = 1.5",
			code: `#include <stdio.h>
{{FUNC}}
double g(double x, double y, double z) { return x + y + z; }
int main() {
\tprintf("%.4f\\n", triple_integral(g, 0,1, 0,1, 0,1, 30));
\treturn 0;
}`,
			expected: "1.5000\n",
		},
	],
};
