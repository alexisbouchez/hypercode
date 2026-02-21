import type { Lesson } from "../../types";

export const work: Lesson = {
	id: "work",
	title: "Work by Variable Force",
	chapterId: "integration-applications",
	content: `## Work by Variable Force

When force varies with position, work is the integral of force over displacement:

$$W = \int_a^b F(x) \, dx$$

### Physical Interpretation

- $F(x)$ is the force (in Newtons) at position $x$ (in meters)
- Work is measured in Joules (N·m)
- **Constant force**: $W = F \cdot d$ (special case where F is uniform)

### Hooke's Law (Spring)

A spring with stiffness $k$ exerts force $F(x) = kx$ when stretched by $x$:

$$W = \int_0^d kx \, dx = \frac{k \cdot d^2}{2}$$

**Example**: spring constant $k=4$, stretched $3\text{m}$:
$W = \int_0^3 4x \, dx = [2x^2]_0^3 = 18 \text{ J}$

### Lifting Variable-Weight Loads

A chain of linear density $\rho$ being lifted: $F(x) = \rho \cdot (L-x)$ where $L$ is chain length. The work to lift the full chain:

$$W = \int_0^L \rho(L-x) \, dx = \frac{\rho L^2}{2}$$

### Your Task

Implement \`double work(double (*force)(double), double a, double b, int n)\` using the midpoint rule.`,

	starterCode: `#include <stdio.h>

double work(double (*force)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += force(a + (i + 0.5) * h);
\t}
\treturn sum * h;
}

double constant_ten(double x) { return 10.0; }

int main() {
\t/* constant force 10 N over 5 m = 50 J */
\tprintf("%.4f\\n", work(constant_ten, 0.0, 5.0, 1000));
\treturn 0;
}`,

	solution: `#include <stdio.h>

double work(double (*force)(double), double a, double b, int n) {
\tdouble h = (b - a) / n;
\tdouble sum = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tsum += force(a + (i + 0.5) * h);
\t}
\treturn sum * h;
}

double constant_ten(double x) { return 10.0; }

int main() {
\tprintf("%.4f\\n", work(constant_ten, 0.0, 5.0, 1000));
\treturn 0;
}`,

	tests: [
		{
			name: "constant force 10 N over 5 m = 50 J",
			expected: "50.0000\n",
		},
		{
			name: "spring k=4 over [0,3]: W = 18 J",
			code: `#include <stdio.h>
{{FUNC}}
double spring(double x) { return 4.0 * x; }
int main() {
\tprintf("%.4f\\n", work(spring, 0.0, 3.0, 10000));
\treturn 0;
}`,
			expected: "18.0000\n",
		},
		{
			name: "variable force 3x² over [0,2]: W = 8 J",
			code: `#include <stdio.h>
{{FUNC}}
double cubic(double x) { return 3.0 * x * x; }
int main() {
\tprintf("%.4f\\n", work(cubic, 0.0, 2.0, 10000));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
		{
			name: "linear force x+1 over [0,4]: W = ∫(x+1)dx = [x²/2+x]_0^4 = 12",
			code: `#include <stdio.h>
{{FUNC}}
double linear(double x) { return x + 1.0; }
int main() {
\tprintf("%.4f\\n", work(linear, 0.0, 4.0, 10000));
\treturn 0;
}`,
			expected: "12.0000\n",
		},
	],
};
