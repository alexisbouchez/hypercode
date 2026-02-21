import type { Lesson } from "../../types";

export const relativisticEnergy: Lesson = {
	id: "relativistic-energy",
	title: "Relativistic Energy",
	chapterId: "dynamics",
	content: `## Relativistic Energy

Einstein's most famous equation gives the **rest energy** of a particle:

$$E_0 = mc^2$$

This is the energy locked in a particle's mass — even at complete rest. The **total relativistic energy** includes the kinetic contribution via the Lorentz factor $\\gamma$:

$$E = \\gamma mc^2$$

The **kinetic energy** is the difference between total and rest energy:

$$K = E - E_0 = (\\gamma - 1)mc^2$$

### Classical Limit

When $v \\ll c$, the Taylor expansion of $\\gamma$ gives:

$$K \\approx \\frac{1}{2}mv^2$$

This recovers the Newtonian result, confirming relativity reduces to classical mechanics at low speeds.

### Your Task

Implement:
- \`rest_energy(m)\` — returns $E_0 = mc^2$
- \`total_energy(m, v)\` — returns $E = \\gamma mc^2$
- \`kinetic_energy(m, v)\` — returns $K = (\\gamma - 1)mc^2$

Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `import math

def rest_energy(m):
    c = 299792458.0
    # E0 = m * c^2
    pass

def total_energy(m, v):
    c = 299792458.0
    # gamma * m * c^2
    pass

def kinetic_energy(m, v):
    c = 299792458.0
    # (gamma - 1) * m * c^2
    pass

print(round(rest_energy(1.0) / 1e16, 4))
print(round(total_energy(1.0, 0.6 * 299792458.0) / rest_energy(1.0), 4))
print(round(kinetic_energy(1.0, 0.8 * 299792458.0) / rest_energy(1.0), 4))
print(round(kinetic_energy(1.0, 0.99 * 299792458.0) / rest_energy(1.0), 4))
`,

	solution: `import math

def rest_energy(m):
    c = 299792458.0
    return m * c ** 2

def total_energy(m, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return gamma * m * c ** 2

def kinetic_energy(m, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return (gamma - 1) * m * c ** 2

print(round(rest_energy(1.0) / 1e16, 4))
print(round(total_energy(1.0, 0.6 * 299792458.0) / rest_energy(1.0), 4))
print(round(kinetic_energy(1.0, 0.8 * 299792458.0) / rest_energy(1.0), 4))
print(round(kinetic_energy(1.0, 0.99 * 299792458.0) / rest_energy(1.0), 4))
`,

	tests: [
		{
			name: "rest_energy(1 kg) / 1e16 = 8.9876 (≈ c² × 10⁻¹⁶)",
			code: `{{FUNC}}
print(round(rest_energy(1.0) / 1e16, 4))`,
			expected: "8.9876\n",
		},
		{
			name: "total_energy at v=0.6c: ratio to rest energy = γ = 1.25",
			code: `{{FUNC}}
print(round(total_energy(1.0, 0.6 * 299792458.0) / rest_energy(1.0), 4))`,
			expected: "1.25\n",
		},
		{
			name: "kinetic_energy at v=0.8c: ratio to rest energy = γ−1 = 2/3 ≈ 0.6667",
			code: `{{FUNC}}
print(round(kinetic_energy(1.0, 0.8 * 299792458.0) / rest_energy(1.0), 4))`,
			expected: "0.6667\n",
		},
		{
			name: "kinetic_energy at v=0.99c: ratio to rest energy = γ−1 ≈ 6.0888",
			code: `{{FUNC}}
print(round(kinetic_energy(1.0, 0.99 * 299792458.0) / rest_energy(1.0), 4))`,
			expected: "6.0888\n",
		},
	],
};
