import type { Lesson } from "../../types";

export const qValue: Lesson = {
	id: "q-value",
	title: "Q-Value of Nuclear Reactions",
	chapterId: "nuclear-reactions",
	content: `## Q-Value of Nuclear Reactions

The **Q-value** of a nuclear reaction is the energy released (or absorbed). It comes from Einstein's $E = mc^2$: the difference in total rest mass between reactants and products converts to kinetic energy.

$$Q = (\\sum m_{\\text{reactants}} - \\sum m_{\\text{products}}) \\times 931.494 \\text{ MeV/u}$$

where atomic mass unit $1\\text{ u} = 931.494$ MeV/$c^2$.

- **$Q > 0$**: exothermic — energy is released (fission, fusion)
- **$Q < 0$**: endothermic — energy must be supplied

### Famous Example: D + T Fusion

$$^2\\text{H} + ^3\\text{H} \\rightarrow ^4\\text{He} + n$$

| Particle | Mass (u) |
|----------|----------|
| Deuteron D | 2.014102 |
| Triton T | 3.016049 |
| Helium-4 | 4.002602 |
| Neutron | 1.008665 |

$$Q = (2.014102 + 3.016049 - 4.002602 - 1.008665) \\times 931.494 = 17.59 \\text{ MeV}$$

### Threshold Energy

For endothermic reactions ($Q < 0$), the beam projectile must carry a minimum **threshold energy**:

$$E_{\\text{th}} = |Q| \\left(1 + \\frac{m_{\\text{beam}}}{m_{\\text{target}}}\\right)$$

### Your Task

Implement:
- \`q_value_MeV(m_reactants_total_u, m_products_total_u)\` — Q in MeV
- \`is_exothermic(m_reactants_total_u, m_products_total_u)\` — True if Q > 0
- \`threshold_energy_MeV(Q_MeV, m_beam_u, m_target_u)\` — threshold energy, or 0.0 if exothermic`,

	starterCode: `def q_value_MeV(m_reactants_total_u, m_products_total_u):
    # Q = (m_reactants - m_products) * 931.494
    pass

def is_exothermic(m_reactants_total_u, m_products_total_u):
    # return True if Q > 0
    pass

def threshold_energy_MeV(Q_MeV, m_beam_u, m_target_u):
    # If Q < 0: E_th = |Q| * (1 + m_beam/m_target)
    # Otherwise: 0.0
    pass
`,

	solution: `def q_value_MeV(m_reactants_total_u, m_products_total_u):
    return (m_reactants_total_u - m_products_total_u) * 931.494

def is_exothermic(m_reactants_total_u, m_products_total_u):
    return q_value_MeV(m_reactants_total_u, m_products_total_u) > 0

def threshold_energy_MeV(Q_MeV, m_beam_u, m_target_u):
    if Q_MeV < 0:
        return abs(Q_MeV) * (1 + m_beam_u / m_target_u)
    return 0.0
`,

	tests: [
		{
			name: "D+T fusion Q-value ≈ 17.5903 MeV",
			code: `{{FUNC}}
print(round(q_value_MeV(2.014102 + 3.016049, 4.002602 + 1.008665), 4))`,
			expected: "17.5903\n",
		},
		{
			name: "U-238 alpha decay Q-value ≈ 4.2709 MeV",
			code: `{{FUNC}}
print(round(q_value_MeV(238.050788, 234.043601 + 4.002602), 4))`,
			expected: "4.2709\n",
		},
		{
			name: "D+T fusion is exothermic",
			code: `{{FUNC}}
print(is_exothermic(2.014102 + 3.016049, 4.002602 + 1.008665))`,
			expected: "True\n",
		},
		{
			name: "threshold energy for p + Li-7 → Be-7 + n ≈ 1.8813 MeV",
			code: `{{FUNC}}
Q = q_value_MeV(1.007825 + 7.016003, 7.016929 + 1.008665)
print(round(threshold_energy_MeV(Q, 1.007825, 7.016003), 4))`,
			expected: "1.8813\n",
		},
	],
};
