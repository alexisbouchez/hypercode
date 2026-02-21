import type { Lesson } from "../../types";

export const rlcTransient: Lesson = {
	id: "rlc_transient",
	title: "RLC Transient (RK4)",
	chapterId: "transient-response",
	content: `## The RLC Circuit

Adding an inductor to the RC circuit creates an **RLC circuit** — the complete second-order system. It can exhibit oscillation.

\`\`\`
Vs ---[R]---[L]---+--- GND
                  |
                 [C]
                  |
                 GND
\`\`\`

### The Governing ODE

By KVL: Vs = V_R + V_L + V_C = R·I + L·dI/dt + (1/C)·∫I dt

Differentiating and substituting I = C·dV_C/dt:

\`\`\`
L·C·V_C'' + R·C·V_C' + V_C = Vs
\`\`\`

This is a **second-order linear ODE**. We solve it numerically using **RK4**.

### Damping Ratio

The behavior depends on α = R/(2L) and ω₀ = 1/√(LC):

| Condition | Behavior |
|-----------|----------|
| α > ω₀ | **Overdamped**: slow exponential approach |
| α = ω₀ | **Critically damped**: fastest approach, no oscillation |
| α < ω₀ | **Underdamped**: oscillates around Vs |

### Solving with RK4

We reduce to two first-order equations. Let x₁ = V_C and x₂ = dV_C/dt:

\`\`\`
dx₁/dt = x₂
dx₂/dt = (Vs − x₁ − R·C·x₂) / (L·C)
\`\`\`

Then apply the RK4 integrator with step h over n steps.

### Your Task

Implement \`double rlc_voltage(double vs, double r, double l, double c, double t, double dt)\` that integrates the RLC circuit from t=0 to t using RK4 with step dt, starting from V_C(0) = 0 and dV_C/dt(0) = 0.

Use \`#include <math.h>\` for any math functions needed.`,

	starterCode: `#include <stdio.h>
#include <math.h>

double rlc_voltage(double vs, double r, double l, double c,
                   double t, double dt) {
\t/* RK4 integration of the RLC ODE */
\tdouble x1 = 0.0; /* V_C */
\tdouble x2 = 0.0; /* dV_C/dt */
\tint steps = (int)(t / dt);
\tfor (int i = 0; i < steps; i++) {
\t\t/* TODO: apply one RK4 step */
\t\t(void)x1; (void)x2;
\t}
\treturn x1;
}

int main() {
\t/* Overdamped: large R → V_C approaches Vs */
\tprintf("%.4f\\n", rlc_voltage(10.0, 100.0, 1.0, 0.01, 15.0, 0.001));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

double rlc_voltage(double vs, double r, double l, double c,
                   double t, double dt) {
\tdouble x1 = 0.0;
\tdouble x2 = 0.0;
\tint steps = (int)(t / dt);
\tfor (int i = 0; i < steps; i++) {
\t\tdouble k1x1 = x2;
\t\tdouble k1x2 = (vs - x1 - r*c*x2) / (l*c);
\t\tdouble k2x1 = x2 + 0.5*dt*k1x2;
\t\tdouble k2x2 = (vs - (x1 + 0.5*dt*k1x1) - r*c*(x2 + 0.5*dt*k1x2)) / (l*c);
\t\tdouble k3x1 = x2 + 0.5*dt*k2x2;
\t\tdouble k3x2 = (vs - (x1 + 0.5*dt*k2x1) - r*c*(x2 + 0.5*dt*k2x2)) / (l*c);
\t\tdouble k4x1 = x2 + dt*k3x2;
\t\tdouble k4x2 = (vs - (x1 + dt*k3x1) - r*c*(x2 + dt*k3x2)) / (l*c);
\t\tx1 += dt*(k1x1 + 2*k2x1 + 2*k3x1 + k4x1) / 6.0;
\t\tx2 += dt*(k1x2 + 2*k2x2 + 2*k3x2 + k4x2) / 6.0;
\t}
\treturn x1;
}

int main() {
\tprintf("%.4f\\n", rlc_voltage(10.0, 100.0, 1.0, 0.01, 15.0, 0.001));
\treturn 0;
}
`,

	tests: [
		{
			name: "overdamped (R=100Ω): V_C → Vs=10 after 5s",
			expected: "10.0000\n",
		},
		{
			name: "at t=0: V_C = 0",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rlc_voltage(10.0, 100.0, 1.0, 0.01, 0.0, 0.001));
\treturn 0;
}`,
			expected: "0.0000\n",
		},
		{
			name: "critically damped (R=20Ω, L=1H, C=0.01F → α=10=ω₀): V_C → 10 after 2s",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rlc_voltage(10.0, 20.0, 1.0, 0.01, 2.0, 0.001));
\treturn 0;
}`,
			expected: "10.0000\n",
		},
		{
			name: "Vs=5V overdamped: V_C → 5 after 5s",
			code: `#include <stdio.h>
#include <math.h>
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rlc_voltage(5.0, 100.0, 1.0, 0.01, 15.0, 0.001));
\treturn 0;
}`,
			expected: "5.0000\n",
		},
	],
};
