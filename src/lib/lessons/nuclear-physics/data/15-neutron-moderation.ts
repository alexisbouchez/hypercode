import type { Lesson } from "../../types";

export const neutronModeration: Lesson = {
	id: "neutron-moderation",
	title: "Neutron Moderation",
	chapterId: "nuclear-energy",
	content: `## Neutron Moderation

Fission of U-235 releases **fast neutrons** with energies around 2 MeV. However, U-235 fissions most efficiently with **thermal neutrons** at around 0.025 eV (room temperature). A **moderator** slows neutrons down through elastic collisions.

### Average Logarithmic Energy Decrement

Each collision reduces a neutron's energy. The average logarithmic energy loss per collision is:

$$\\xi = 1 + \\frac{(A-1)^2}{2A} \\ln\\!\\left(\\frac{A-1}{A+1}\\right)$$

For hydrogen ($A = 1$): $\\xi = 1$ (special case, largest possible energy loss per collision).

| Moderator | $A$ | $\\xi$ |
|-----------|-----|-------|
| Hydrogen ($^1$H) | 1 | 1.000 |
| Deuterium ($^2$H) | 2 | 0.725 |
| Carbon (graphite) | 12 | 0.158 |

### Number of Collisions to Thermalise

Starting from $E_0 = 2 \\times 10^6$ eV (fast neutron), reaching $E_f = 0.025$ eV (thermal):

$$n = \\frac{\\ln(E_0/E_f)}{\\xi}$$

Hydrogen requires only ~18 collisions; graphite requires ~115.

### Average Energy After One Collision

For a neutron of energy $E$ colliding with a nucleus of mass $A$ (in neutron mass units):

$$E_{\\text{avg}} = E \\cdot \\frac{A^2 + 1}{(A + 1)^2}$$

### Your Task

All constants must be defined **inside** each function. Use \`import math\` for \`math.log\`.

- \`log_energy_decrement(A)\` — returns $\\xi$ for mass number $A$ (return \`1.0\` for $A = 1$)
- \`collisions_to_thermalize(A, E0_eV=2e6, Ef_eV=0.025)\` — number of collisions needed
- \`energy_after_collision(E, A)\` — average energy after one elastic collision`,

	starterCode: `import math

def log_energy_decrement(A):
    # Special case A=1: xi = 1.0
    # General: 1 + (A-1)^2 / (2*A) * ln((A-1)/(A+1))
    pass

def collisions_to_thermalize(A, E0_eV=2e6, Ef_eV=0.025):
    # n = ln(E0/Ef) / xi
    pass

def energy_after_collision(E, A):
    # E_avg = E * (A^2 + 1) / (A + 1)^2
    pass
`,

	solution: `import math

def log_energy_decrement(A):
    if A == 1:
        return 1.0
    return 1 + (A - 1)**2 / (2 * A) * math.log((A - 1) / (A + 1))

def collisions_to_thermalize(A, E0_eV=2e6, Ef_eV=0.025):
    if A == 1:
        xi = 1.0
    else:
        xi = 1 + (A - 1)**2 / (2 * A) * math.log((A - 1) / (A + 1))
    return math.log(E0_eV / Ef_eV) / xi

def energy_after_collision(E, A):
    return E * (A**2 + 1) / (A + 1)**2
`,

	tests: [
		{
			name: "log_energy_decrement(1) = 1.0 (hydrogen is the ideal moderator)",
			code: `{{FUNC}}
print(log_energy_decrement(1))`,
			expected: "1.0\n",
		},
		{
			name: "log_energy_decrement(2) ≈ 0.725347 (deuterium)",
			code: `{{FUNC}}
print(round(log_energy_decrement(2), 6))`,
			expected: "0.725347\n",
		},
		{
			name: "collisions_to_thermalize(1) ≈ 18 (hydrogen)",
			code: `{{FUNC}}
print(round(collisions_to_thermalize(1)))`,
			expected: "18\n",
		},
		{
			name: "collisions_to_thermalize(12) ≈ 115 (carbon/graphite)",
			code: `{{FUNC}}
print(round(collisions_to_thermalize(12)))`,
			expected: "115\n",
		},
	],
};
