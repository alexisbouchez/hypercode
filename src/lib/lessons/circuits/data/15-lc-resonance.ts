import type { Lesson } from "../../types";

export const lcResonance: Lesson = {
	id: "lc_resonance",
	title: "LC Resonant Frequency",
	chapterId: "ac-and-filters",
	content: `## LC Resonance

When an inductor and capacitor are connected together, they form a **resonant circuit** — energy oscillates between the magnetic field (inductor) and electric field (capacitor).

### The Resonant Frequency

\`\`\`
f₀ = 1 / (2π · √(L · C))
\`\`\`

Or in angular frequency:

\`\`\`
ω₀ = 1 / √(L · C)    (rad/s)
\`\`\`

At resonance:
- The inductive reactance equals the capacitive reactance: X_L = X_C
- In a series RLC: impedance is minimized → maximum current
- In a parallel RLC: impedance is maximized → minimum current

### Physical Intuition

Think of a pendulum: energy alternates between kinetic (inductor: ½LI²) and potential (capacitor: ½CV²). The resonant frequency is the natural "swing rate."

### Derivation

At resonance: ωL = 1/(ωC)  →  ω² = 1/(LC)  →  ω = 1/√(LC)  →  f = 1/(2π√(LC))

### Bandwidth and Q Factor

In a real RLC circuit with resistance R:

\`\`\`
Q = (1/R) · √(L/C)    (quality factor)
BW = f₀ / Q           (bandwidth)
\`\`\`

A high Q means a narrow, sharp resonance peak — good for radio tuning.

### Examples

| L | C | f₀ |
|---|---|----|
| 1H | 1F | **0.1592 Hz** |
| 1mH | 1μF | **5033 Hz** (audio range) |
| 100μH | 100pF | **1.59 MHz** (AM radio) |
| 1μH | 1pF | **159 MHz** (FM radio) |

### Applications

- **Radio tuning**: adjust C to select a station
- **Filters**: bandpass and notch filters
- **Oscillators**: crystal oscillators, LC oscillators
- **Wireless charging**: resonant coupling

### Your Task

Implement \`double lc_resonance(double l, double c)\` that returns the resonant frequency f₀ in Hz.

Use \`#include <math.h>\` for \`sqrt\` and define \`PI\`.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double lc_resonance(double l, double c) {
\t/* f0 = 1 / (2 * PI * sqrt(L * C)) */
\treturn 0.0;
}

int main() {
\t/* L=1H, C=1F → f0 = 1/(2π) ≈ 0.1592 Hz */
\tprintf("%.4f\\n", lc_resonance(1.0, 1.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double lc_resonance(double l, double c) {
\treturn 1.0 / (2.0 * PI * sqrt(l * c));
}

int main() {
\tprintf("%.4f\\n", lc_resonance(1.0, 1.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "L=1H, C=1F → 0.1592 Hz",
			expected: "0.1592\n",
		},
		{
			name: "L=0.001H, C=0.000001F → 5032.9 Hz",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tprintf("%.1f\\n", lc_resonance(0.001, 1e-6));
\treturn 0;
}`,
			expected: "5032.9\n",
		},
		{
			name: "doubling L halves f₀ by √2 ≈ 0.7071",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tdouble f1 = lc_resonance(1.0, 1.0);
\tdouble f2 = lc_resonance(2.0, 1.0);
\tprintf("%.4f\\n", f2 / f1);
\treturn 0;
}`,
			expected: "0.7071\n",
		},
		{
			name: "L=0.0001H, C=1e-10F → f₀ ≈ 1591549 Hz ≈ 1.59 MHz",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tprintf("%.0f\\n", lc_resonance(1e-4, 1e-10));
\treturn 0;
}`,
			expected: "1591549\n",
		},
	],
};
