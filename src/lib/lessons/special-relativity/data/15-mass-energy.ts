import type { Lesson } from "../../types";

export const massEnergy: Lesson = {
	id: "mass-energy",
	title: "Mass-Energy Equivalence",
	chapterId: "dynamics",
	content: `## Mass-Energy Equivalence

Mass and energy are two faces of the same quantity. The rest energy $E_0 = mc^2$ is staggeringly large:

- **1 gram** of matter: $E_0 = 10^{-3} \\times (3 \\times 10^8)^2 \\approx 9 \\times 10^{13}$ J — roughly equivalent to 21 kilotons of TNT.

### Nuclear Binding Energy

The mass of a nucleus is **less** than the sum of its constituent protons and neutrons. This **mass defect** $\\Delta m$ is released as energy:

$$\\Delta E = \\Delta m \\cdot c^2$$

### Fusion in the Sun

Four protons (total mass $4 \\times 1.6726 \\times 10^{-27}$ kg) fuse to form a helium-4 nucleus (mass $6.6447 \\times 10^{-27}$ kg). The mass defect:

$$\\Delta m = 4m_p - m_{\\text{He}} = 4.57 \\times 10^{-29}\\ \\text{kg}$$

releases $\\Delta E \\approx 4.1 \\times 10^{-12}$ J per fusion event — the energy that powers the Sun.

### Your Task

Implement:
- \`mass_to_energy(m)\` — returns $E = mc^2$ in joules
- \`energy_to_mass(E)\` — returns $m = E/c^2$ in kg
- \`binding_energy(mass_parts, mass_nucleus)\` — returns $(\\Delta m) c^2$ in joules

Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `def mass_to_energy(m):
    c = 299792458.0
    # m * c^2
    pass

def energy_to_mass(E):
    c = 299792458.0
    # E / c^2
    pass

def binding_energy(mass_parts, mass_nucleus):
    c = 299792458.0
    # (mass_parts - mass_nucleus) * c^2
    pass

print(round(mass_to_energy(1e-3) / 1e12, 4))
print(round(energy_to_mass(mass_to_energy(1.0)), 4))
print(round(binding_energy(4 * 1.6726e-27, 6.6447e-27) / 1e-12, 2))
print(round(energy_to_mass(1.602e-19) / 1e-36, 4))
`,

	solution: `def mass_to_energy(m):
    c = 299792458.0
    return m * c ** 2

def energy_to_mass(E):
    c = 299792458.0
    return E / c ** 2

def binding_energy(mass_parts, mass_nucleus):
    c = 299792458.0
    delta_m = mass_parts - mass_nucleus
    return delta_m * c ** 2

print(round(mass_to_energy(1e-3) / 1e12, 4))
print(round(energy_to_mass(mass_to_energy(1.0)), 4))
print(round(binding_energy(4 * 1.6726e-27, 6.6447e-27) / 1e-12, 2))
print(round(energy_to_mass(1.602e-19) / 1e-36, 4))
`,

	tests: [
		{
			name: "1 gram to energy / 1e12 TJ = 89.8755",
			code: `{{FUNC}}
print(round(mass_to_energy(1e-3) / 1e12, 4))`,
			expected: "89.8755\n",
		},
		{
			name: "Round-trip: energy_to_mass(mass_to_energy(1.0)) = 1.0",
			code: `{{FUNC}}
print(round(energy_to_mass(mass_to_energy(1.0)), 4))`,
			expected: "1.0\n",
		},
		{
			name: "Solar fusion binding energy (4p → He-4) / 1e-12 J ≈ 4.11",
			code: `{{FUNC}}
print(round(binding_energy(4 * 1.6726e-27, 6.6447e-27) / 1e-12, 2))`,
			expected: "4.11\n",
		},
		{
			name: "Mass of 1 eV (1.602e-19 J) / 1e-36 kg ≈ 1.7825",
			code: `{{FUNC}}
print(round(energy_to_mass(1.602e-19) / 1e-36, 4))`,
			expected: "1.7825\n",
		},
	],
};
