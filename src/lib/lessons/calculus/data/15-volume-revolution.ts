import type { Lesson } from "../../types";

export const volumeRevolution: Lesson = {
	id: "volume-revolution",
	title: "Volume of Revolution",
	chapterId: "integral-applications",
	content: `## Volume of Revolution

Rotating a curve \`y = f(x)\` around the **x-axis** sweeps out a solid. We can find its volume using the **disk method**.

### Disk Method

At each \`x\`, the cross-section is a disk with radius \`f(x)\` and area \`π·f(x)²\`. Integrating these areas:

\`\`\`
V = π · ∫_a^b [f(x)]² dx
\`\`\`

### Washer Method

Rotating the region between two curves \`f(x)\` (outer) and \`g(x)\` (inner):

\`\`\`
V = π · ∫_a^b ([f(x)]² - [g(x)]²) dx
\`\`\`

Each cross-section is a **washer** (disk with hole).

### Classic Examples

**Cylinder**: rotate \`f(x) = r\` (constant) on \`[0, h]\`:
\`\`\`
V = π·r²·h
\`\`\`

**Cone**: rotate \`f(x) = rx/h\` (line) on \`[0, h]\`:
\`\`\`
V = π · ∫₀ʰ (rx/h)² dx = (π·r²/h²) · h³/3 = π·r²·h/3
\`\`\`

**Sphere**: rotate \`f(x) = √(r²-x²)\` on \`[-r, r]\`:
\`\`\`
V = π · ∫₋ᵣʳ (r²-x²) dx = (4/3)π·r³
\`\`\`

### Numerical Implementation

Use the midpoint rule:
\`\`\`c
#define PI 3.14159265358979
for each midpoint x:
    sum += f(x) * f(x)
return PI * sum * h
\`\`\`

### Your Task

Implement \`double volume_disk(double (*f)(double), double a, double b, int n)\` that computes \`π ∫_a^b f(x)² dx\` using the midpoint rule.`,

	starterCode: `#include <stdio.h>
#define PI 3.14159265358979

double volume_disk(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * h;
\t\tdouble r = f(x);
\t\tsum += r * r;
\t}
\treturn PI * sum * h;
}

double unit(double x) { return 1.0; }

int main() {
\t/* cylinder of radius 1, height 2: V = pi*1*2 ~ 6.2832 */
\tprintf("%.4f\\n", volume_disk(unit, 0.0, 2.0, 10000));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#define PI 3.14159265358979

double volume_disk(double (*f)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * h;
\t\tdouble r = f(x);
\t\tsum += r * r;
\t}
\treturn PI * sum * h;
}

double unit(double x) { return 1.0; }

int main() {
\tprintf("%.4f\\n", volume_disk(unit, 0.0, 2.0, 10000));
\treturn 0;
}
`,

	tests: [
		{
			name: "cylinder radius=1 height=2: V = 2*pi ~ 6.2832",
			expected: "6.2832\n",
		},
		{
			name: "cylinder radius=2 height=3: V = 12*pi ~ 37.6991",
			code: `#include <stdio.h>
#define PI 3.14159265358979
{{FUNC}}
double two(double x) { return 2.0; }
int main() {
\tprintf("%.4f\\n", volume_disk(two, 0.0, 3.0, 10000));
\treturn 0;
}`,
			expected: "37.6991\n",
		},
		{
			name: "cone: rotate y=x on [0,3]: V = pi*9 ~ 28.2743",
			code: `#include <stdio.h>
#define PI 3.14159265358979
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", volume_disk(id, 0.0, 3.0, 10000));
\treturn 0;
}`,
			expected: "28.2743\n",
		},
		{
			name: "rotate y=x on [0,1]: V = pi/3 ~ 1.0472",
			code: `#include <stdio.h>
#define PI 3.14159265358979
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", volume_disk(id, 0.0, 1.0, 10000));
\treturn 0;
}`,
			expected: "1.0472\n",
		},
	],
};
