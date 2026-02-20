import type { Lesson } from "../../types";

export const rlStep: Lesson = {
	id: "rl_step",
	title: "RL Step Response",
	chapterId: "transient-response",
	content: `## The RL Circuit: Step Response

An inductor **resists changes in current** — the dual of a capacitor, which resists changes in voltage.

When a voltage Vs is suddenly applied to a series RL circuit:

\`\`\`
Vs ---[R]---[L]--- GND
\`\`\`

The current builds up exponentially:

\`\`\`
I(t) = (Vs / R) · (1 − e^(−t / τ))
\`\`\`

Where **τ = L / R** is the time constant (seconds).

### The Duality with RC

| RC circuit | RL circuit |
|-----------|-----------|
| Voltage V(t) builds up | Current I(t) builds up |
| Final value: Vs (open cap) | Final value: Vs/R (short inductor) |
| τ = R·C | τ = L/R |
| Capacitor blocks DC (steady state) | Inductor passes DC (steady state) |

### Time Constant τ = L/R

At t = τ: I = (Vs/R) · (1 − 1/e) ≈ **63.2%** of final current

| t | I(t) |
|---|------|
| 0 | 0 |
| τ | 63.2% of Vs/R |
| 5τ | 99.3% of Vs/R |

### Physical Intuition

The inductor's voltage is V_L = L · dI/dt. Initially, current is zero and V_L = Vs (all voltage across inductor). As current builds up, the voltage drop across R increases, reducing V_L, which slows the rate of change of current.

### Applications

- Motor drivers (inductors are motor windings)
- Switching power supplies
- RF filters and chokes
- Flyback converters

### Your Task

Implement \`double rl_step(double vs, double r, double l, double t)\` that returns the current I(t) for a series RL circuit with step voltage Vs.

Use \`#include <math.h>\` for \`exp\`.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double rl_step(double vs, double r, double l, double t) {
\t/* I(t) = (Vs/R) * (1 - exp(-R*t/L)) */
\treturn 0.0;
}

int main() {
\t/* Vs=10V, R=5Ω, L=1H, t=0 → I=0 */
\tprintf("%.4f\\n", rl_step(10.0, 5.0, 1.0, 0.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double rl_step(double vs, double r, double l, double t) {
\treturn (vs / r) * (1.0 - exp(-r * t / l));
}

int main() {
\tprintf("%.4f\\n", rl_step(10.0, 5.0, 1.0, 0.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "t=0: I starts at 0",
			expected: "0.0000\n",
		},
		{
			name: "t→∞: I = Vs/R = 10/5 = 2A",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rl_step(10.0, 5.0, 1.0, 100.0));
\treturn 0;
}`,
			expected: "2.0000\n",
		},
		{
			name: "t→∞: I = 12/4 = 3A",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rl_step(12.0, 4.0, 2.0, 100.0));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "at t=τ=L/R: I ≈ 63.2% of Vs/R → 6.3212",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\t/* τ=1s, Vs=10V, R=1Ω → I(τ)=10*(1-1/e) */
\tprintf("%.4f\\n", rl_step(10.0, 1.0, 1.0, 1.0));
\treturn 0;
}`,
			expected: "6.3212\n",
		},
	],
};
