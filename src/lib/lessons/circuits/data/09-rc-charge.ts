import type { Lesson } from "../../types";

export const rcCharge: Lesson = {
	id: "rc_charge",
	title: "RC Charging",
	chapterId: "transient-response",
	content: `## The RC Circuit: Charging

An RC circuit (resistor + capacitor in series) is the simplest circuit with memory — its behavior depends on history, not just the current input.

\`\`\`
    Vs ---[R]---+---[C]--- GND
                |
               V(t)
\`\`\`

When a voltage Vs is applied, the capacitor charges exponentially:

\`\`\`
V(t) = Vs · (1 − e^(−t / τ))
\`\`\`

Where **τ = R · C** is the **time constant** (seconds).

### The Time Constant

At t = τ: V = Vs · (1 − 1/e) ≈ **0.632 · Vs** (63.2% of final voltage)

| t | V(t)/Vs |
|---|---------|
| 0 | 0% |
| 1τ | 63.2% |
| 2τ | 86.5% |
| 3τ | 95.0% |
| 5τ | 99.3% (considered "fully charged") |

### Physical Intuition

Initially, the capacitor is empty — all voltage drops across R and current flows freely. As the cap charges, its voltage rises, reducing the driving voltage (Vs − V_cap), which slows the current. The charging slows exponentially.

### General Form (arbitrary initial voltage V₀)

\`\`\`
V(t) = Vs + (V₀ − Vs) · e^(−t / τ)
\`\`\`

This covers both charging (V₀ < Vs) and partial charging (V₀ > 0).

### Applications

- Timing circuits (555 timer)
- Debouncing switches
- Power supply filtering
- Analog delays and integrators

### Your Task

Implement \`double rc_charge(double vs, double v0, double r, double c, double t)\` that returns the capacitor voltage at time \`t\`.

Use \`#include <math.h>\` for \`exp\`.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double rc_charge(double vs, double v0, double r, double c, double t) {
\t/* V(t) = Vs + (V0 - Vs) * exp(-t / (R*C)) */
\treturn 0.0;
}

int main() {
\t/* Charging from 0V to 10V, R=1Ω, C=1F, t=0 → 0V */
\tprintf("%.4f\\n", rc_charge(10.0, 0.0, 1.0, 1.0, 0.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double rc_charge(double vs, double v0, double r, double c, double t) {
\tdouble tau = r * c;
\treturn vs + (v0 - vs) * exp(-t / tau);
}

int main() {
\tprintf("%.4f\\n", rc_charge(10.0, 0.0, 1.0, 1.0, 0.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "t=0: V starts at V0=0",
			expected: "0.0000\n",
		},
		{
			name: "t→∞: V converges to Vs=10",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rc_charge(10.0, 0.0, 1.0, 1.0, 100.0));
\treturn 0;
}`,
			expected: "10.0000\n",
		},
		{
			name: "at t=τ: V ≈ 63.2% of Vs → 3.1606",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\t/* τ = 1s, Vs=5V, V0=0 → V(τ) = 5*(1-1/e) */
\tprintf("%.4f\\n", rc_charge(5.0, 0.0, 1.0, 1.0, 1.0));
\treturn 0;
}`,
			expected: "3.1606\n",
		},
		{
			name: "discharging: Vs=0, V0=10, t→∞ → 0V",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rc_charge(0.0, 10.0, 1.0, 1.0, 100.0));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
	],
};
