import type { Lesson } from "../../types";

export const rcHighpass: Lesson = {
	id: "rc_highpass",
	title: "RC High-Pass Filter",
	chapterId: "ac-and-filters",
	content: `## The RC High-Pass Filter

Swapping R and C swaps the behavior — now C is in series and R shunts to ground:

\`\`\`
Vin ---[C]---+--- Vout
             |
            [R]
             |
            GND
\`\`\`

At **low frequencies**: C has high impedance → blocks signal → Vout ≈ 0

At **high frequencies**: C has low impedance → passes signal → Vout ≈ Vin

### Voltage Gain (Magnitude)

\`\`\`
|H(f)| = (2πfRC) / sqrt(1 + (2πfRC)²)
\`\`\`

Or writing ωRC = 2πfRC:

\`\`\`
|H| = ωRC / sqrt(1 + (ωRC)²)
\`\`\`

### Complementary to Low-Pass

Notice: \`|H_low|² + |H_high|² = 1\`

The low-pass and high-pass filters are **complementary** — they share the same cutoff frequency f_c = 1/(2πRC) and together account for all signal power.

| f | |H_high| | Meaning |
|---|---------|---------|
| 0 | 0.0000 | DC fully blocked |
| f_c | 0.7071 | −3 dB (same as low-pass cutoff) |
| 10·f_c | 0.9950 | mostly passed |
| ∞ | 1.0000 | high freq passes fully |

### Applications

- **Coupling capacitors**: block DC bias between amplifier stages
- **Bass cut** in audio equalizers
- **Differentiator** (at low frequencies, output ∝ dVin/dt)
- **AC coupling** oscilloscope inputs

### Your Task

Implement \`double rc_highpass(double f, double r, double c)\` that returns the voltage gain magnitude |H(f)|.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double rc_highpass(double f, double r, double c) {
\t/* |H| = (2*pi*f*R*C) / sqrt(1 + (2*pi*f*R*C)^2) */
\treturn 0.0;
}

int main() {
\t/* f=0 (DC): blocked → 0 */
\tprintf("%.4f\\n", rc_highpass(0.0, 1.0, 1.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double rc_highpass(double f, double r, double c) {
\tdouble wrc = 2.0 * PI * f * r * c;
\treturn wrc / sqrt(1.0 + wrc*wrc);
}

int main() {
\tprintf("%.4f\\n", rc_highpass(0.0, 1.0, 1.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "f=0 (DC): blocked → 0.0000",
			expected: "0.0000\n",
		},
		{
			name: "f=fc: gain = 0.7071 (same −3dB as low-pass)",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rc_highpass(1.0/(2.0*PI), 1.0, 1.0));
\treturn 0;
}`,
			expected: "0.7071\n",
		},
		{
			name: "f=10*fc: gain ≈ 0.9950 (mostly passed)",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rc_highpass(10.0/(2.0*PI), 1.0, 1.0));
\treturn 0;
}`,
			expected: "0.9950\n",
		},
		{
			name: "complementarity: |Hlow|²+|Hhigh|²=1 → sum=1",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
double rc_lowpass(double f, double r, double c) {
\tdouble wrc = 2.0*PI*f*r*c;
\treturn 1.0 / sqrt(1.0 + wrc*wrc);
}
int main() {
\tdouble f = 500.0, r = 1.0, c = 1.0;
\tdouble hl = rc_lowpass(f, r, c);
\tdouble hh = rc_highpass(f, r, c);
\tprintf("%.4f\\n", hl*hl + hh*hh);
\treturn 0;
}`,
			expected: "1.0000\n",
		},
	],
};
