import type { Lesson } from "../../types";

export const rcLowpass: Lesson = {
	id: "rc_lowpass",
	title: "RC Low-Pass Filter",
	chapterId: "ac-and-filters",
	content: `## Frequency Response

So far we've analyzed DC circuits (constant voltages). Real signals have **frequency content** — they oscillate. Different frequencies pass through a circuit differently.

### The RC Low-Pass Filter

\`\`\`
Vin ---[R]---+--- Vout
             |
            [C]
             |
            GND
\`\`\`

The capacitor's impedance is Xc = 1/(jωC), where ω = 2πf. At low frequencies, Xc is large → Vout ≈ Vin. At high frequencies, Xc is small → Vout ≈ 0.

### Voltage Gain (Magnitude)

\`\`\`
|H(f)| = 1 / sqrt(1 + (2πfRC)²)
\`\`\`

Or equivalently using ω = 2πf:

\`\`\`
|H| = 1 / sqrt(1 + (ωRC)²)
\`\`\`

### The Cutoff Frequency

At f_c = 1/(2πRC), gain = 1/√2 ≈ 0.7071 — the **−3 dB point**. This is where power is halved.

| f | |H| | Meaning |
|---|-----|---------|
| 0 | 1.0000 | DC passes through |
| f_c | 0.7071 | −3 dB (half power) |
| 10·f_c | 0.0995 | mostly blocked |
| 100·f_c | 0.0100 | almost completely blocked |

### Applications

- Audio: remove high-frequency hiss
- ADC anti-aliasing: remove frequencies above Nyquist
- Power supply: smooth rectified AC into near-DC
- Signal smoothing / averaging

### Your Task

Implement \`double rc_lowpass(double f, double r, double c)\` that returns the voltage gain magnitude |H(f)|.

Use \`#include <math.h>\` for \`sqrt\` and define \`PI\`.`,

	starterCode: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double rc_lowpass(double f, double r, double c) {
\t/* |H| = 1 / sqrt(1 + (2*pi*f*R*C)^2) */
\treturn 0.0;
}

int main() {
\t/* f=0 (DC) → gain=1 */
\tprintf("%.4f\\n", rc_lowpass(0.0, 1.0, 1.0));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

#define PI 3.14159265358979

double rc_lowpass(double f, double r, double c) {
\tdouble wrc = 2.0 * PI * f * r * c;
\treturn 1.0 / sqrt(1.0 + wrc*wrc);
}

int main() {
\tprintf("%.4f\\n", rc_lowpass(0.0, 1.0, 1.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "f=0 (DC): gain = 1.0000",
			expected: "1.0000\n",
		},
		{
			name: "f=fc=1/(2π): gain = 1/√2 = 0.7071 (−3dB)",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\t/* fc = 1/(2*pi*R*C), R=1, C=1 → fc=1/(2*pi) */
\tprintf("%.4f\\n", rc_lowpass(1.0/(2.0*PI), 1.0, 1.0));
\treturn 0;
}`,
			expected: "0.7071\n",
		},
		{
			name: "f=10*fc: gain ≈ 0.0995 (mostly blocked)",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tprintf("%.4f\\n", rc_lowpass(10.0/(2.0*PI), 1.0, 1.0));
\treturn 0;
}`,
			expected: "0.0995\n",
		},
		{
			name: "R=1kΩ, C=1μF: fc=159Hz. At fc → 0.7071",
			code: `#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979
{{FUNC}}
int main() {
\tdouble r = 1000.0, c = 1e-6;
\tdouble fc = 1.0 / (2.0 * PI * r * c);
\tprintf("%.4f\\n", rc_lowpass(fc, r, c));
\treturn 0;
}`,
			expected: "0.7071\n",
		},
	],
};
