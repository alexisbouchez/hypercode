import type { Lesson } from "../../types";

export const nodeVoltage: Lesson = {
	id: "node_v",
	title: "Node Voltage (KCL)",
	chapterId: "circuit-analysis",
	content: `## Kirchhoff's Current Law

**Kirchhoff's Current Law (KCL)**: the sum of all currents entering a node equals the sum leaving. Equivalently, the **net current at any node is zero**:

\`\`\`
Σ I = 0
\`\`\`

### Solving for an Unknown Node Voltage

Consider a floating node connected to n known voltage sources through resistors:

\`\`\`
V1 --[R1]--+
V2 --[R2]--+--- V_node  (unknown)
V3 --[R3]--+
\`\`\`

Current from source k into the node: Iₖ = (Vₖ − V_node) / Rₖ

By KCL, all these currents sum to zero:

\`\`\`
Σ (Vₖ − V_node) / Rₖ = 0
\`\`\`

Solving for V_node (the **weighted average** of the source voltages):

\`\`\`
V_node = Σ(Vₖ / Rₖ) / Σ(1 / Rₖ)
\`\`\`

Each source is weighted by its **conductance** (1/R). A lower resistance pulls the node voltage closer to its source.

### Examples

| Sources | Resistors | V_node |
|---------|-----------|--------|
| [12V, 0V] | [1Ω, 1Ω] | **6V** (simple average) |
| [12V, 0V] | [1Ω, 2Ω] | **8V** (12V closer — lower R₁) |
| [9V, 3V, 0V] | [1Ω, 1Ω, 1Ω] | **4V** (equal weight average) |

### Your Task

Implement \`double node_voltage(double *v, double *r, int n)\` that returns the unknown node voltage using the KCL formula above.`,

	starterCode: `#include <stdio.h>

double node_voltage(double *v, double *r, int n) {
\t/* V_node = sum(v[i]/r[i]) / sum(1/r[i]) */
\treturn 0.0;
}

int main() {
\tdouble v[] = {12.0, 0.0};
\tdouble r[] = {1.0,  1.0};
\t/* Equal resistors → simple average: 6V */
\tprintf("%.4f\\n", node_voltage(v, r, 2));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

double node_voltage(double *v, double *r, int n) {
\tdouble num = 0.0, den = 0.0;
\tfor (int i = 0; i < n; i++) {
\t\tnum += v[i] / r[i];
\t\tden += 1.0 / r[i];
\t}
\treturn num / den;
}

int main() {
\tdouble v[] = {12.0, 0.0};
\tdouble r[] = {1.0,  1.0};
\tprintf("%.4f\\n", node_voltage(v, r, 2));
\treturn 0;
}
`,

	tests: [
		{
			name: "[12V,0V] via [1Ω,1Ω] → 6V",
			expected: "6.0000\n",
		},
		{
			name: "[12V,0V] via [1Ω,2Ω] → 8V (closer to 12V)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble v[] = {12.0, 0.0};
\tdouble r[] = {1.0, 2.0};
\tprintf("%.4f\\n", node_voltage(v, r, 2));
\treturn 0;
}`,
			expected: "8.0000\n",
		},
		{
			name: "[9V,3V,0V] via [1Ω,1Ω,1Ω] → 4V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble v[] = {9.0, 3.0, 0.0};
\tdouble r[] = {1.0, 1.0, 1.0};
\tprintf("%.4f\\n", node_voltage(v, r, 3));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "[10V,0V] via [2Ω,3Ω] → 6V",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tdouble v[] = {10.0, 0.0};
\tdouble r[] = {2.0, 3.0};
\tprintf("%.4f\\n", node_voltage(v, r, 2));
\treturn 0;
}`,
			expected: "6.0000\n",
		},
	],
};
