import type { Lesson } from "../../types";

export const surfaceArea: Lesson = {
	id: "surface-area",
	title: "Surface Area of Revolution",
	chapterId: "integration-applications",
	content: `## Surface Area of Revolution

Rotating \`y = f(x)\` around the **x-axis** creates a surface. Its area is:

\`\`\`
SA = 2π · ∫_a^b f(x) · √(1 + [f'(x)]²) dx
\`\`\`

### Why It Works

Each strip of width \`dx\` wraps into a ribbon of radius \`f(x)\` and slant height \`√(1+f'²)dx\`. The ribbon's area is \`2π·f(x)·√(1+f'²)dx\`.

### Classic Examples

**Cylinder** \`f(x) = r\` on \`[0, L]\`: \`f' = 0\`, so \`SA = 2πrL\`

**Cone** \`f(x) = x\` on \`[0, 1]\`: \`f' = 1\`, so:
\`\`\`
SA = 2π ∫_0^1 x·√2 dx = 2π√2 · [x²/2]_0^1 = π√2 ≈ 4.4429
\`\`\`

### Numerical Approach

\`\`\`c
#define PI 3.14159265358979
for each midpoint x:
    double fp = (f(x+h) - f(x-h)) / (2*h);
    sum += f(x) * my_sqrt(1.0 + fp * fp);
return 2.0 * PI * sum * dx;
\`\`\`

### Your Task

Implement \`double surface_area(double (*f)(double), double a, double b, int n, double h)\`.`,

	starterCode: `#include <stdio.h>
#define PI 3.14159265358979

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double surface_area(double (*f)(double), double a, double b, int n, double h) {
\tdouble dx = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * dx;
\t\tdouble fp = (f(x + h) - f(x - h)) / (2.0 * h);
\t\tsum += f(x) * my_sqrt(1.0 + fp * fp);
\t}
\treturn 2.0 * PI * sum * dx;
}

double unit(double x) { return 1.0; }

int main() {
\t/* cylinder r=1, L=3: SA = 2*pi*1*3 = 6*pi ~ 18.8496 */
\tprintf("%.4f\\n", surface_area(unit, 0.0, 3.0, 10000, 1e-5));
\treturn 0;
}`,

	solution: `#include <stdio.h>
#define PI 3.14159265358979

static double my_sqrt(double x) {
\tdouble r = x;
\tfor (int i = 0; i < 50; i++) r = (r + x / r) / 2.0;
\treturn r;
}

double surface_area(double (*f)(double), double a, double b, int n, double h) {
\tdouble dx = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tdouble x = a + (i + 0.5) * dx;
\t\tdouble fp = (f(x + h) - f(x - h)) / (2.0 * h);
\t\tsum += f(x) * my_sqrt(1.0 + fp * fp);
\t}
\treturn 2.0 * PI * sum * dx;
}

double unit(double x) { return 1.0; }

int main() {
\tprintf("%.4f\\n", surface_area(unit, 0.0, 3.0, 10000, 1e-5));
\treturn 0;
}`,

	tests: [
		{
			name: "cylinder r=1 on [0,3]: SA = 6π ≈ 18.8496",
			expected: "18.8496\n",
		},
		{
			name: "cone y=x on [0,1]: SA = π√2 ≈ 4.4429",
			code: `#include <stdio.h>
#define PI 3.14159265358979
static double my_sqrt(double x) { double r=x; for(int i=0;i<50;i++) r=(r+x/r)/2.0; return r; }
{{FUNC}}
double id(double x) { return x; }
int main() {
\tprintf("%.4f\\n", surface_area(id, 0.0, 1.0, 10000, 1e-5));
\treturn 0;
}`,
			expected: "4.4429\n",
		},
		{
			name: "cylinder r=3 on [0,2]: SA = 12π ≈ 37.6991",
			code: `#include <stdio.h>
#define PI 3.14159265358979
static double my_sqrt(double x) { double r=x; for(int i=0;i<50;i++) r=(r+x/r)/2.0; return r; }
{{FUNC}}
double three(double x) { return 3.0; }
int main() {
\tprintf("%.4f\\n", surface_area(three, 0.0, 2.0, 10000, 1e-5));
\treturn 0;
}`,
			expected: "37.6991\n",
		},
		{
			name: "cylinder r=2 on [0,1]: SA = 4π ≈ 12.5664",
			code: `#include <stdio.h>
#define PI 3.14159265358979
static double my_sqrt(double x) { double r=x; for(int i=0;i<50;i++) r=(r+x/r)/2.0; return r; }
{{FUNC}}
double two(double x) { return 2.0; }
int main() {
\tprintf("%.4f\\n", surface_area(two, 0.0, 1.0, 10000, 1e-5));
\treturn 0;
}`,
			expected: "12.5664\n",
		},
	],
};
