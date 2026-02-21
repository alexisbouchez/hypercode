import type { Lesson } from "../../types";

export const energyMomentum: Lesson = {
	id: "energy-momentum",
	title: "Energy-Momentum Relation",
	chapterId: "dynamics",
	content: `## Energy-Momentum Relation

Energy and momentum unite into a 4-vector in special relativity. The **energy-momentum relation** is the relativistic counterpart of $E = p^2/2m$:

$$E^2 = (pc)^2 + (mc^2)^2$$

This relation is **Lorentz-invariant** — every observer agrees on it, regardless of their relative motion. Unlike $E = \\gamma mc^2$ or $p = \\gamma mv$, it does not reference the particle's velocity at all.

### Special Cases

- **At rest** ($p = 0$): recovers $E = mc^2$
- **Photon** ($m = 0$): gives $E = pc$, so light carries momentum proportional to its energy

### Your Task

Implement:
- \`energy_from_momentum(m, p)\` — computes $E = \\sqrt{(pc)^2 + (mc^2)^2}$
- \`momentum_from_energy(m, E)\` — inverts to get $p = \\sqrt{E^2 - (mc^2)^2} / c$

Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `import math

def energy_from_momentum(m, p):
    c = 299792458.0
    # sqrt((p*c)^2 + (m*c^2)^2)
    pass

def momentum_from_energy(m, E):
    c = 299792458.0
    # sqrt(E^2 - (m*c^2)^2) / c
    pass

print(round(energy_from_momentum(1.0, 0.0) / 1e16, 4))
print(round(energy_from_momentum(0.0, 1.0), 4))
print(round(momentum_from_energy(0.0, energy_from_momentum(0.0, 2.0)), 4))
print(round(energy_from_momentum(1.0, 0.75 * 299792458.0) / 299792458.0 ** 2, 4))
`,

	solution: `import math

def energy_from_momentum(m, p):
    c = 299792458.0
    return math.sqrt((p * c) ** 2 + (m * c ** 2) ** 2)

def momentum_from_energy(m, E):
    c = 299792458.0
    return math.sqrt(E ** 2 - (m * c ** 2) ** 2) / c

print(round(energy_from_momentum(1.0, 0.0) / 1e16, 4))
print(round(energy_from_momentum(0.0, 1.0), 4))
print(round(momentum_from_energy(0.0, energy_from_momentum(0.0, 2.0)), 4))
print(round(energy_from_momentum(1.0, 0.75 * 299792458.0) / 299792458.0 ** 2, 4))
`,

	tests: [
		{
			name: "At rest (p=0): E = mc², ratio to 1e16 = 8.9876",
			code: `{{FUNC}}
print(round(energy_from_momentum(1.0, 0.0) / 1e16, 4))`,
			expected: "8.9876\n",
		},
		{
			name: "Photon (m=0, p=1): E = pc = c = 299792458.0",
			code: `{{FUNC}}
print(round(energy_from_momentum(0.0, 1.0), 4))`,
			expected: "299792458.0\n",
		},
		{
			name: "Round-trip photon: momentum_from_energy(energy_from_momentum(m=0, p=2)) = 2.0",
			code: `{{FUNC}}
print(round(momentum_from_energy(0.0, energy_from_momentum(0.0, 2.0)), 4))`,
			expected: "2.0\n",
		},
		{
			name: "v=0.6c, m=1 kg, p=0.75c: E/c² = γm = 1.25",
			code: `{{FUNC}}
print(round(energy_from_momentum(1.0, 0.75 * 299792458.0) / 299792458.0 ** 2, 4))`,
			expected: "1.25\n",
		},
	],
};
