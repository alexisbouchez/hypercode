import type { Lesson } from "../../types";

export const lcResonanceLesson: Lesson = {
	id: "lc-resonance",
	title: "LC Resonant Frequency",
	chapterId: "em-induction",
	content: `## LC Oscillator

An inductor (L) and capacitor (C) in series or parallel exchange energy back and forth at a natural resonant frequency:

$$f = \frac{1}{2\pi\sqrt{LC}}$$

- **f** — resonant frequency (Hz)
- **L** — inductance (H)
- **C** — capacitance (F)

### The Energy Seesaw

The capacitor stores energy in its electric field ($E = \frac{1}{2}CV^2$); the inductor stores energy in its magnetic field ($E = \frac{1}{2}LI^2$). At resonance they alternate — like a pendulum swapping kinetic and potential energy — at frequency f.

### Relation to the Pendulum

| LC Circuit | Pendulum |
|-----------|---------|
| Charge q | Displacement x |
| Inductance L | Mass m |
| 1/C | Spring constant k |
| $f = \frac{1}{2\pi\sqrt{LC}}$ | $f = \frac{1}{2\pi}\sqrt{\frac{k}{m}}$ |

### Radio Tuning

Tuning a radio dial changes C in an LC circuit, selecting the resonant frequency that matches the desired station.

### Examples

| L (H) | C (F) | f (Hz) |
|-------|-------|--------|
| 1×10⁻³ | 1×10⁻⁶ | **5032.92** |
| 1×10⁻² | 1×10⁻⁴ | **159.15** |
| 0.1 | 1×10⁻³ | **15.92** |
| 1×10⁻³ | 1×10⁻⁷ | **15915.49** |

### Your Task

Implement \`lc_resonance(L, C)\` returning the resonant frequency in Hz.`,

	starterCode: `import math

def lc_resonance(L, C):
    # f = 1 / (2 * pi * sqrt(L * C))
    return 0

print(f"{lc_resonance(1e-3, 1e-6):.2f}")    # 5032.92
print(f"{lc_resonance(1e-2, 1e-4):.2f}")    # 159.15
print(f"{lc_resonance(0.1, 1e-3):.2f}")     # 15.92
print(f"{lc_resonance(1e-3, 1e-7):.2f}")    # 15915.49
`,

	solution: `import math

def lc_resonance(L, C):
    return 1 / (2 * math.pi * math.sqrt(L * C))

print(f"{lc_resonance(1e-3, 1e-6):.2f}")    # 5032.92
print(f"{lc_resonance(1e-2, 1e-4):.2f}")    # 159.15
print(f"{lc_resonance(0.1, 1e-3):.2f}")     # 15.92
print(f"{lc_resonance(1e-3, 1e-7):.2f}")    # 15915.49
`,

	tests: [
		{
			name: "L=1 mH, C=1 μF → f=5032.92 Hz",
			code: `{{FUNC}}
print(f"{lc_resonance(1e-3, 1e-6):.2f}")`,
			expected: "5032.92\n",
		},
		{
			name: "L=10 mH, C=100 μF → f=159.15 Hz",
			code: `{{FUNC}}
print(f"{lc_resonance(1e-2, 1e-4):.2f}")`,
			expected: "159.15\n",
		},
		{
			name: "L=0.1 H, C=1 mF → f=15.92 Hz",
			code: `{{FUNC}}
print(f"{lc_resonance(0.1, 1e-3):.2f}")`,
			expected: "15.92\n",
		},
		{
			name: "L=1 mH, C=100 nF → f=15915.49 Hz",
			code: `{{FUNC}}
print(f"{lc_resonance(1e-3, 1e-7):.2f}")`,
			expected: "15915.49\n",
		},
	],
};
