import type { Lesson } from "../../types";

export const fusionEnergy: Lesson = {
	id: "fusion-energy",
	title: "Nuclear Fusion Energy",
	chapterId: "nuclear-energy",
	content: `## Nuclear Fusion Energy

**Fusion** combines light nuclei to form a heavier nucleus, releasing far more energy per unit mass than fission. It powers the Sun and is the goal of tokamak research reactors.

### Deuterium–Tritium (D-T) Fusion

The most promising reaction for terrestrial reactors:

$$^2\\text{H} + ^3\\text{H} \\rightarrow ^4\\text{He} + n$$

| Particle | Mass (u) |
|----------|----------|
| $^2$H (deuterium) | 2.014102 |
| $^3$H (tritium) | 3.016049 |
| $^4$He (alpha) | 4.002602 |
| $n$ (neutron) | 1.008665 |

$$Q = (2.014102 + 3.016049 - 4.002602 - 1.008665) \\times 931.494 \\approx 17.59 \\text{ MeV}$$

### Energy per Kilogram

The energy per kg of D-T reactant mixture is:

$$E/\\text{kg} = \\frac{Q_{\\text{J}}}{m_{\\text{pair}}} = \\frac{Q \\times 1.602 \\times 10^{-13}}{(m_D + m_T) \\times 1.66054 \\times 10^{-27}}$$

This gives roughly $3.4 \\times 10^{14}$ J/kg — about 7 million times more than gasoline ($4.6 \\times 10^7$ J/kg).

### Lawson Criterion

For a D-T plasma to sustain fusion, the product of ion density $n$ and energy confinement time $\\tau_E$ must exceed:

$$n \\tau_E > \\frac{10^{20}}{T_{\\text{keV}}} \\quad [\\text{m}^{-3}\\text{s}]$$

At $T = 10$ keV, the threshold is $n\\tau_E > 10^{19}$ m$^{-3}$s.

### Your Task

All constants must be defined **inside** each function.

- \`dt_fusion_q()\` — D-T Q-value in MeV (no parameters)
- \`fusion_energy_per_kg(Q_MeV, m_reactants_u_per_reaction)\` — energy in J/kg ($1$ u $= 1.66054 \\times 10^{-27}$ kg)
- \`lawson_criterion_DT(T_keV)\` — minimum $n\\tau_E$ threshold in m$^{-3}$s`,

	starterCode: `import math

def dt_fusion_q():
    # mD=2.014102, mT=3.016049, mHe4=4.002602, mn=1.008665
    # Q = mass_defect * 931.494
    pass

def fusion_energy_per_kg(Q_MeV, m_reactants_u_per_reaction):
    # 1 MeV = 1.602e-13 J, 1 u = 1.66054e-27 kg
    pass

def lawson_criterion_DT(T_keV):
    # threshold: 1e20 / T_keV
    pass
`,

	solution: `import math

def dt_fusion_q():
    mD = 2.014102
    mT = 3.016049
    mHe4 = 4.002602
    mn = 1.008665
    return (mD + mT - mHe4 - mn) * 931.494

def fusion_energy_per_kg(Q_MeV, m_reactants_u_per_reaction):
    MeV_to_J = 1.602e-13
    u_to_kg = 1.66054e-27
    return Q_MeV * MeV_to_J / (m_reactants_u_per_reaction * u_to_kg)

def lawson_criterion_DT(T_keV):
    return 1e20 / T_keV
`,

	tests: [
		{
			name: "D-T fusion Q-value ≈ 17.5903 MeV",
			code: `{{FUNC}}
print(round(dt_fusion_q(), 4))`,
			expected: "17.5903\n",
		},
		{
			name: "D-T fusion energy per kg ≈ 3.3737e+14 J/kg",
			code: `{{FUNC}}
Q = dt_fusion_q()
print('%.4e' % fusion_energy_per_kg(Q, 2.014102 + 3.016049))`,
			expected: "3.3737e+14\n",
		},
		{
			name: "Lawson criterion at 10 keV = 1e19, at 100 keV = 1e18",
			code: `{{FUNC}}
print('%.2e' % lawson_criterion_DT(10))
print('%.2e' % lawson_criterion_DT(100))`,
			expected: "1.00e+19\n1.00e+18\n",
		},
		{
			name: "D-T fusion is ~7 million times more energetic than gasoline",
			code: `{{FUNC}}
Q = dt_fusion_q()
e_dt = fusion_energy_per_kg(Q, 2.014102 + 3.016049)
print(round(e_dt / 4.6e7, 0))`,
			expected: "7334126.0\n",
		},
	],
};
