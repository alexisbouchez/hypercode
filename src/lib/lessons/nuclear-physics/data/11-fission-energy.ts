import type { Lesson } from "../../types";

export const fissionEnergy: Lesson = {
	id: "fission-energy",
	title: "Nuclear Fission Energy",
	chapterId: "nuclear-energy",
	content: `## Nuclear Fission Energy

When a heavy nucleus like uranium-235 absorbs a slow neutron, it splits into two smaller fragments and releases an enormous amount of energy. This is **nuclear fission**.

### The Q-value

The energy released per reaction is given by the **Q-value**, computed from the mass difference between reactants and products:

$$Q = (\\sum m_{\\text{reactants}} - \\sum m_{\\text{products}}) \\times 931.494 \\text{ MeV/u}$$

The conversion factor $931.494$ MeV/u comes from Einstein's $E = mc^2$ applied to atomic mass units.

### U-235 Fission Example

A typical reaction:

$$^{235}\\text{U} + n \\rightarrow ^{92}\\text{Kr} + ^{141}\\text{Ba} + 3n$$

| Particle | Mass (u) |
|----------|----------|
| $^{235}$U | 235.043930 |
| $n$ | 1.008665 |
| $^{92}$Kr | 91.926156 |
| $^{141}$Ba | 140.914411 |

Mass of reactants: $235.043930 + 1.008665 = 236.052595$ u  
Mass of products: $91.926156 + 140.914411 + 3 \\times 1.008665 = 235.857897$ u  
$Q \\approx 173.3$ MeV per fission event.

### Energy Density

One kilogram of U-235 contains $N = N_A / M$ atoms (where $M = 235 \\times 10^{-3}$ kg/mol). Multiplying by $Q$ (converted to joules via $1 \\text{ MeV} = 1.602 \\times 10^{-13}$ J) gives the total energy per kilogram — roughly $10^{13}$ J/kg, millions of times more than chemical explosives (~$4.6 \\times 10^{6}$ J/kg for TNT).

### Your Task

Implement the three functions below. All constants must be defined **inside** each function.

- \`fission_q_value(m_fuel_u, m_products_total_u)\` — Q-value in MeV
- \`fission_energy_per_kg(Q_MeV, molar_mass_kg_per_mol)\` — energy released per kg of fuel in J/kg ($N_A = 6.022 \\times 10^{23}$, $1$ MeV $= 1.602 \\times 10^{-13}$ J)
- \`fission_vs_chemical(Q_MeV, molar_mass_kg_per_mol)\` — ratio of fission energy to TNT ($4.6 \\times 10^6$ J/kg)`,

	starterCode: `import math

def fission_q_value(m_fuel_u, m_products_total_u):
    # Q = mass_defect * 931.494 MeV
    pass

def fission_energy_per_kg(Q_MeV, molar_mass_kg_per_mol):
    # NA = 6.022e23, 1 MeV = 1.602e-13 J
    pass

def fission_vs_chemical(Q_MeV, molar_mass_kg_per_mol):
    # TNT energy density = 4.6e6 J/kg
    pass
`,

	solution: `import math

def fission_q_value(m_fuel_u, m_products_total_u):
    return (m_fuel_u - m_products_total_u) * 931.494

def fission_energy_per_kg(Q_MeV, molar_mass_kg_per_mol):
    NA = 6.022e23
    MeV_to_J = 1.602e-13
    return Q_MeV * MeV_to_J * NA / molar_mass_kg_per_mol

def fission_vs_chemical(Q_MeV, molar_mass_kg_per_mol):
    NA = 6.022e23
    MeV_to_J = 1.602e-13
    TNT_J_per_kg = 4.6e6
    energy = Q_MeV * MeV_to_J * NA / molar_mass_kg_per_mol
    return energy / TNT_J_per_kg
`,

	tests: [
		{
			name: "U-235 fission Q-value ≈ 173.2886 MeV",
			code: `{{FUNC}}
m_fuel = 235.043930 + 1.008665
m_products = 91.926156 + 140.914411 + 3 * 1.008665
print(round(fission_q_value(m_fuel, m_products), 4))`,
			expected: "173.2886\n",
		},
		{
			name: "U-235 fission energy per kg ≈ 7.1139e+13 J/kg",
			code: `{{FUNC}}
m_fuel = 235.043930 + 1.008665
m_products = 91.926156 + 140.914411 + 3 * 1.008665
Q = fission_q_value(m_fuel, m_products)
print('%.4e' % fission_energy_per_kg(Q, 235e-3))`,
			expected: "7.1139e+13\n",
		},
		{
			name: "U-235 fission is ~15 million times more energetic than TNT",
			code: `{{FUNC}}
m_fuel = 235.043930 + 1.008665
m_products = 91.926156 + 140.914411 + 3 * 1.008665
Q = fission_q_value(m_fuel, m_products)
print(round(fission_vs_chemical(Q, 235e-3), 0))`,
			expected: "15464918.0\n",
		},
		{
			name: "Pu-239 approximate fission Q-value ≈ 186.2988 MeV",
			code: `{{FUNC}}
print(round(fission_q_value(240.054, 239.854), 4))`,
			expected: "186.2988\n",
		},
	],
};
