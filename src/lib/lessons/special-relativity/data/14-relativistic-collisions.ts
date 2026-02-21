import type { Lesson } from "../../types";

export const relativisticCollisions: Lesson = {
	id: "relativistic-collisions",
	title: "Relativistic Collisions",
	chapterId: "dynamics",
	content: `## Relativistic Collisions

In relativistic collisions, **4-momentum is conserved**:

$$p_1^\\mu + p_2^\\mu = p_3^\\mu + p_4^\\mu$$

### Center-of-Mass Energy

The **Mandelstam variable $s$** gives the square of the center-of-mass energy:

$$s = (E_1 + E_2)^2 - (\\vec{p}_1 + \\vec{p}_2)^2 c^2$$

This is Lorentz-invariant — it takes the same value in every reference frame.

### Threshold Energy

The minimum beam energy needed to create a new particle of mass $M$ when hitting a stationary target of mass $m$ (both initial particles also have mass $m$) is:

$$E_{\\text{threshold}} = \\frac{(4mM + M^2)\\,c^2}{2m}$$

This comes from requiring enough center-of-mass energy to produce all final-state particles at rest: $\\sqrt{s_{\\min}} = (2m + M)c^2$.

### Your Task

Implement:
- \`cm_energy_sq(E1, p1, E2, p2)\` — computes $s = (E_1+E_2)^2 - (p_1+p_2)^2 c^2$ (1D momenta)
- \`threshold_energy(m, M)\` — returns the threshold beam energy in J

Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `import math

def cm_energy_sq(E1, p1, E2, p2):
    c = 299792458.0
    # (E1+E2)^2 - (p1+p2)^2 * c^2
    pass

def threshold_energy(m, M):
    c = 299792458.0
    # (4*m*M + M^2) * c^2 / (2*m)
    pass

print(round(cm_energy_sq(1.0, 1.0 / 299792458.0, 1.0, -1.0 / 299792458.0), 4))
print(round(threshold_energy(1.0, 1.0) / 299792458.0 ** 2, 4))
print(round(threshold_energy(1.0, 0.0), 4))
print(round(cm_energy_sq(299792458.0 ** 2, 0.0, 299792458.0 ** 2, 0.0) / 299792458.0 ** 4, 4))
`,

	solution: `import math

def cm_energy_sq(E1, p1, E2, p2):
    c = 299792458.0
    return (E1 + E2) ** 2 - (p1 + p2) ** 2 * c ** 2

def threshold_energy(m, M):
    c = 299792458.0
    return (4 * m * M + M ** 2) * c ** 2 / (2 * m)

print(round(cm_energy_sq(1.0, 1.0 / 299792458.0, 1.0, -1.0 / 299792458.0), 4))
print(round(threshold_energy(1.0, 1.0) / 299792458.0 ** 2, 4))
print(round(threshold_energy(1.0, 0.0), 4))
print(round(cm_energy_sq(299792458.0 ** 2, 0.0, 299792458.0 ** 2, 0.0) / 299792458.0 ** 4, 4))
`,

	tests: [
		{
			name: "Head-on photons (p=±1/c): s = (E1+E2)² = 4.0 J²",
			code: `{{FUNC}}
print(round(cm_energy_sq(1.0, 1.0 / 299792458.0, 1.0, -1.0 / 299792458.0), 4))`,
			expected: "4.0\n",
		},
		{
			name: "Threshold energy (m=M=1 kg): E_threshold / c² = 2.5",
			code: `{{FUNC}}
print(round(threshold_energy(1.0, 1.0) / 299792458.0 ** 2, 4))`,
			expected: "2.5\n",
		},
		{
			name: "Threshold energy to create nothing (M=0): E_threshold = 0.0",
			code: `{{FUNC}}
print(round(threshold_energy(1.0, 0.0), 4))`,
			expected: "0.0\n",
		},
		{
			name: "Two particles at rest (E=mc², p=0): s / c⁴ = 4.0",
			code: `{{FUNC}}
print(round(cm_energy_sq(299792458.0 ** 2, 0.0, 299792458.0 ** 2, 0.0) / 299792458.0 ** 4, 4))`,
			expected: "4.0\n",
		},
	],
};
