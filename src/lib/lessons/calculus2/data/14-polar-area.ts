import type { Lesson } from "../../types";

export const polarArea: Lesson = {
	id: "polar-area",
	title: "Polar Area",
	chapterId: "parametric-and-polar",
	content: `## Polar Area

In **polar coordinates**, a curve is defined by radius as a function of angle: \`r = r(θ)\`.

The area enclosed by a polar curve from \`θ = a\` to \`θ = b\`:

\`\`\`
A = ½ · ∫_a^b [r(θ)]² dθ
\`\`\`

### Why the Formula Works

Each infinitesimal slice at angle \`θ\` is a sector with radius \`r(θ)\` and angle \`dθ\`. A sector's area is \`½r²dθ\` (fraction of circle area \`πr²\`).

### Classic Examples

**Full circle** \`r = R\` on \`[0, 2π]\`:
\`\`\`
A = ½ ∫_0^{2π} R² dθ = ½ · R² · 2π = πR²
\`\`\`

**Semicircle** \`r = 1\` on \`[0, π]\`:
\`\`\`
A = ½ ∫_0^π 1 dθ = π/2 ≈ 1.5708
\`\`\`

**Rose curves** and **limaçons**: more complex integrands requiring numerical integration.

### Numerical Approach

Apply the midpoint rule:
\`\`\`c
#define PI 3.14159265358979
double h = (b - a) / n;
for each midpoint θ:
    double r = r_fn(θ);
    sum += r * r;
return 0.5 * sum * h;
\`\`\`

### Your Task

Implement \`double polar_area(double (*r_fn)(double), double a, double b, int n)\`.`,

	starterCode: `#include <stdio.h>
#define PI 3.14159265358979

double polar_area(double (*r_fn)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble theta = a + (i + 0.5) * h;
\t\tdouble r = r_fn(theta);
\t\tsum += r * r;
\t}
\treturn 0.5 * sum * h;
}

double two(double theta) { return 2.0; }

int main() {
\t/* full circle r=2: A = pi*4 ~ 12.5664 */
\tprintf("%.4f\\n", polar_area(two, 0.0, 2.0 * PI, 100000));
\treturn 0;
}`,

	solution: `#include <stdio.h>
#define PI 3.14159265358979

double polar_area(double (*r_fn)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble theta = a + (i + 0.5) * h;
\t\tdouble r = r_fn(theta);
\t\tsum += r * r;
\t}
\treturn 0.5 * sum * h;
}

double two(double theta) { return 2.0; }

int main() {
\tprintf("%.4f\\n", polar_area(two, 0.0, 2.0 * PI, 100000));
\treturn 0;
}`,

	tests: [
		{
			name: "full circle r=2 on [0,2π]: A = 4π ≈ 12.5664",
			expected: "12.5664\n",
		},
		{
			name: "semicircle r=1 on [0,π]: A = π/2 ≈ 1.5708",
			code: `#include <stdio.h>
#define PI 3.14159265358979
{{FUNC}}
double one(double theta) { return 1.0; }
int main() {
\tprintf("%.4f\\n", polar_area(one, 0.0, PI, 100000));
\treturn 0;
}`,
			expected: "1.5708\n",
		},
		{
			name: "full circle r=3 on [0,2π]: A = 9π ≈ 28.2743",
			code: `#include <stdio.h>
#define PI 3.14159265358979
{{FUNC}}
double three(double theta) { return 3.0; }
int main() {
\tprintf("%.4f\\n", polar_area(three, 0.0, 2.0 * PI, 100000));
\treturn 0;
}`,
			expected: "28.2743\n",
		},
		{
			name: "quarter circle r=2 on [0,π/2]: A = π ≈ 3.1416",
			code: `#include <stdio.h>
#define PI 3.14159265358979
{{FUNC}}
double two(double theta) { return 2.0; }
int main() {
\tprintf("%.4f\\n", polar_area(two, 0.0, PI / 2.0, 100000));
\treturn 0;
}`,
			expected: "3.1416\n",
		},
	],
};
