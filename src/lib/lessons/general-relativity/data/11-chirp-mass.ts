import type { Lesson } from "../../types";

export const chirpMass: Lesson = {
	id: "chirp-mass",
	title: "Chirp Mass",
	chapterId: "gravitational-waves",
	content: `## Chirp Mass

When two compact objects spiral together and merge, they emit gravitational waves whose frequency sweeps upward — a **chirp**. The rate at which that frequency evolves is controlled by a single combination of the two masses called the **chirp mass**:

$$\\mathcal{M}_c = \\frac{(m_1 m_2)^{3/5}}{(m_1 + m_2)^{1/5}}$$

This is the most precisely measured parameter in gravitational-wave observations. For GW150914 (the first detection, two ~30 solar-mass black holes) LIGO measured $\\mathcal{M}_c \\approx 28.3\\,M_\\odot$ to sub-percent precision, even though individual masses were uncertain by ~10%.

### Why the Chirp Mass?

The power radiated in gravitational waves depends on the masses through $\\mathcal{M}_c$ at leading post-Newtonian order. Measuring the frequency sweep $\\dot{f}$ directly gives $\\mathcal{M}_c$ without needing to know $m_1$ and $m_2$ separately.

### Related Mass Parameters

Two other combinations appear frequently:

**Symmetric mass ratio** (measures how equal the masses are, maximum 0.25 for equal masses):
$$\\eta = \\frac{m_1 m_2}{(m_1 + m_2)^2}$$

**Mass ratio** (by convention $m_1 \\geq m_2$, so $q \\leq 1$):
$$q = \\frac{m_2}{m_1}$$

### Solar Mass

One solar mass: $M_\\odot = 1.989 \\times 10^{30}$ kg.

### Your Task

Implement three functions using the formulae above. All mass parameters must be in kg (or any consistent unit). No physical constants are needed — these are purely algebraic.`,

	starterCode: `import math

def chirp_mass(m1, m2):
    # TODO: return (m1*m2)^(3/5) / (m1+m2)^(1/5)
    pass

def symmetric_mass_ratio(m1, m2):
    # TODO: return m1*m2 / (m1+m2)^2
    pass

def mass_ratio(m1, m2):
    # TODO: return m2/m1  (convention: m1 >= m2, so q <= 1)
    pass

M_sun = 1.989e30
print(f"{chirp_mass(30*M_sun, 30*M_sun):.4e}")
print(round(symmetric_mass_ratio(30*M_sun, 30*M_sun), 4))
print(f"{chirp_mass(36*M_sun, 29*M_sun):.4e}")
print(round(mass_ratio(36*M_sun, 29*M_sun), 4))
`,

	solution: `import math

def chirp_mass(m1, m2):
    return (m1*m2)**(3/5) / (m1+m2)**(1/5)

def symmetric_mass_ratio(m1, m2):
    return m1*m2 / (m1+m2)**2

def mass_ratio(m1, m2):
    return m2/m1

M_sun = 1.989e30
print(f"{chirp_mass(30*M_sun, 30*M_sun):.4e}")
print(round(symmetric_mass_ratio(30*M_sun, 30*M_sun), 4))
print(f"{chirp_mass(36*M_sun, 29*M_sun):.4e}")
print(round(mass_ratio(36*M_sun, 29*M_sun), 4))
`,

	tests: [
		{
			name: "chirp_mass of equal 30 solar-mass binary (GW150914-like)",
			code: `{{FUNC}}
M_sun = 1.989e30
print(f"{chirp_mass(30*M_sun, 30*M_sun):.4e}")`,
			expected: "5.1946e+31\n",
		},
		{
			name: "symmetric_mass_ratio = 0.25 for equal masses",
			code: `{{FUNC}}
M_sun = 1.989e30
print(round(symmetric_mass_ratio(30*M_sun, 30*M_sun), 4))`,
			expected: "0.25\n",
		},
		{
			name: "chirp_mass for asymmetric binary (36 + 29 solar masses)",
			code: `{{FUNC}}
M_sun = 1.989e30
print(f"{chirp_mass(36*M_sun, 29*M_sun):.4e}")`,
			expected: "5.5882e+31\n",
		},
		{
			name: "mass_ratio q = m2/m1 for 36+29 solar-mass binary",
			code: `{{FUNC}}
M_sun = 1.989e30
print(round(mass_ratio(36*M_sun, 29*M_sun), 4))`,
			expected: "0.8056\n",
		},
	],
};
