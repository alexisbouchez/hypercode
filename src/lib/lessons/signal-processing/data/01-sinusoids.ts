import type { Lesson } from "../../types";

export const sinusoids: Lesson = {
	id: "sinusoids",
	title: "Sinusoids",
	chapterId: "fundamentals",
	content: `## Sinusoids

The **sinusoid** is the fundamental building block of signal processing. Every real-world signal — audio, radio, seismic — can be decomposed into a sum of sinusoids by the Fourier theorem.

### The Sinusoidal Signal

A continuous sinusoidal signal is described by:

$$x(t) = A \\sin(2\\pi f t + \\phi)$$

where:
- $A$ = **amplitude** (peak value)
- $f$ = **frequency** in Hz (cycles per second)
- $t$ = **time** in seconds
- $\\phi$ = **phase offset** in radians

### Angular Frequency

It is often convenient to write the angular frequency:

$$\\omega = 2\\pi f \\quad \\text{(radians per second)}$$

so the signal becomes $x(t) = A \\sin(\\omega t + \\phi)$.

### Example

A 440 Hz sinusoid (concert A) with amplitude 1 and zero phase:

$$x(t) = \\sin(2\\pi \\cdot 440 \\cdot t)$$

At $t = 1/1760$ seconds (quarter cycle), $x = \\sin(\\pi/2) = 1.0$ — the peak.

### Your Task

Implement:
- \`sinusoid(A, f, t, phi=0)\` — returns $A \\sin(2\\pi f t + \\phi)$
- \`angular_frequency(f)\` — returns $\\omega = 2\\pi f$

\`\`\`python
import math

def sinusoid(A, f, t, phi=0):
    return A * math.sin(2 * math.pi * f * t + phi)

def angular_frequency(f):
    return 2 * math.pi * f

# 440 Hz sine at t=0 (zero crossing)
print(round(sinusoid(1.0, 440.0, 0.0), 4))   # 0.0
# At quarter period: t = 1/(4*440)
print(round(sinusoid(1.0, 440.0, 1/1760), 4)) # 1.0
print(round(angular_frequency(60.0), 4))       # 376.9911
\`\`\``,

	starterCode: `import math

def sinusoid(A, f, t, phi=0):
    # Return A * sin(2*pi*f*t + phi)
    pass

def angular_frequency(f):
    # Return 2*pi*f
    pass

print(round(sinusoid(1.0, 1.0, 0.25), 4))
print(round(angular_frequency(1.0), 4))
`,

	solution: `import math

def sinusoid(A, f, t, phi=0):
    return A * math.sin(2 * math.pi * f * t + phi)

def angular_frequency(f):
    return 2 * math.pi * f

print(round(sinusoid(1.0, 1.0, 0.25), 4))
print(round(angular_frequency(1.0), 4))
`,

	tests: [
		{
			name: "sinusoid(1.0, 1.0, 0.25) = 1.0 (quarter period, peak)",
			code: `{{FUNC}}
print(round(sinusoid(1.0, 1.0, 0.25), 4))`,
			expected: "1.0\n",
		},
		{
			name: "sinusoid(2.0, 440.0, 0.0) = 0.0 (zero crossing)",
			code: `{{FUNC}}
print(round(sinusoid(2.0, 440.0, 0.0), 4))`,
			expected: "0.0\n",
		},
		{
			name: "angular_frequency(1.0) = 6.2832",
			code: `{{FUNC}}
print(round(angular_frequency(1.0), 4))`,
			expected: "6.2832\n",
		},
		{
			name: "angular_frequency(60.0) = 376.9911",
			code: `{{FUNC}}
print(round(angular_frequency(60.0), 4))`,
			expected: "376.9911\n",
		},
		{
			name: "sinusoid with phase offset pi/2 at t=0 = A",
			code: `{{FUNC}}
import math
print(round(sinusoid(3.0, 1.0, 0.0, math.pi/2), 4))`,
			expected: "3.0\n",
		},
	],
};
