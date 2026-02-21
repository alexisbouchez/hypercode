import type { Lesson } from "../../types";

export const cmEnergy: Lesson = {
	id: "cm-energy",
	title: "Center-of-Mass Energy",
	chapterId: "kinematics",
	content: `## Center-of-Mass Energy

The **center-of-mass energy** $\\sqrt{s}$ is the total available energy in the CM frame — the maximum energy that can go into creating new particles. The Mandelstam variable $s$ is:

$$s = (p_1 + p_2)^2 = (E_1 + E_2)^2 - |\\mathbf{p}_1 + \\mathbf{p}_2|^2$$

### Collider Mode (head-on, equal beams)

When two equal-energy beams collide head-on, the momenta cancel:

$$\\sqrt{s} = 2 E_{\\text{beam}}$$

The LHC at 6.5 TeV per beam delivers $\\sqrt{s} = 13$ TeV. This is why colliders are so powerful compared to fixed-target experiments.

### Fixed-Target Mode

With a beam hitting a stationary target (natural units, $c = 1$):

$$s = m_{\\text{beam}}^2 + m_{\\text{target}}^2 + 2 E_{\\text{beam}}\\, m_{\\text{target}}$$

The $\\sqrt{s}$ only grows as $\\sqrt{E_{\\text{beam}}}$, far slower than the linear growth of the collider case.

### Threshold Energy

The minimum beam kinetic energy $T_{\\text{thresh}}$ to produce a set of final-state particles with total mass $M_{\\text{prod}}$ is found by setting $s = M_{\\text{prod}}^2$ at threshold:

$$T_{\\text{thresh}} = \\frac{M_{\\text{prod}}^2 - m_{\\text{beam}}^2 - m_{\\text{target}}^2}{2\\, m_{\\text{target}}} - m_{\\text{beam}}$$

### Your Task

Implement (all masses and energies in GeV, natural units):

- \`collider_cm_energy(E_beam_GeV)\` — returns $\\sqrt{s} = 2 E_{\\text{beam}}$
- \`fixed_target_cm_energy(E_beam_GeV, m_beam_GeV, m_target_GeV)\` — returns $\\sqrt{s}$
- \`threshold_kinetic_energy(m_beam_GeV, m_target_GeV, m_products_total_GeV)\` — returns minimum kinetic energy $T_{\\text{thresh}}$`,

	starterCode: `import math

def collider_cm_energy(E_beam_GeV):
    # sqrt(s) = 2 * E_beam
    pass

def fixed_target_cm_energy(E_beam_GeV, m_beam_GeV, m_target_GeV):
    # s = m_beam^2 + m_target^2 + 2*E_beam*m_target
    pass

def threshold_kinetic_energy(m_beam_GeV, m_target_GeV, m_products_total_GeV):
    # T_thresh = (M_prod^2 - m_beam^2 - m_target^2) / (2*m_target) - m_beam
    pass

print(collider_cm_energy(6500))
m_p = 0.938272
print(round(fixed_target_cm_energy(1000, m_p, m_p), 4))
print(round(threshold_kinetic_energy(m_p, m_p, 2*m_p + 0.135), 4))
print(round(fixed_target_cm_energy(450, m_p, m_p), 4))
`,

	solution: `import math

def collider_cm_energy(E_beam_GeV):
    return 2 * E_beam_GeV

def fixed_target_cm_energy(E_beam_GeV, m_beam_GeV, m_target_GeV):
    s = m_beam_GeV**2 + m_target_GeV**2 + 2*E_beam_GeV*m_target_GeV
    return math.sqrt(s)

def threshold_kinetic_energy(m_beam_GeV, m_target_GeV, m_products_total_GeV):
    E_thresh = (m_products_total_GeV**2 - m_beam_GeV**2 - m_target_GeV**2) / (2*m_target_GeV)
    return E_thresh - m_beam_GeV

print(collider_cm_energy(6500))
m_p = 0.938272
print(round(fixed_target_cm_energy(1000, m_p, m_p), 4))
print(round(threshold_kinetic_energy(m_p, m_p, 2*m_p + 0.135), 4))
print(round(fixed_target_cm_energy(450, m_p, m_p), 4))
`,

	tests: [
		{
			name: "LHC collider_cm_energy(6500) = 13000.0 GeV (13 TeV)",
			code: `{{FUNC}}
print(collider_cm_energy(6500))`,
			expected: "13000\n",
		},
		{
			name: "fixed_target_cm_energy: 1000 GeV proton on stationary proton",
			code: `{{FUNC}}
m_p = 0.938272
print(round(fixed_target_cm_energy(1000, m_p, m_p), 4))`,
			expected: "43.3394\n",
		},
		{
			name: "threshold_kinetic_energy for p+p → p+p+π⁰ production",
			code: `{{FUNC}}
m_p = 0.938272
print(round(threshold_kinetic_energy(m_p, m_p, 2*m_p + 0.135), 4))`,
			expected: "0.2797\n",
		},
		{
			name: "fixed_target_cm_energy: LHC injection at 450 GeV, proton on proton",
			code: `{{FUNC}}
m_p = 0.938272
print(round(fixed_target_cm_energy(450, m_p, m_p), 4))`,
			expected: "29.0896\n",
		},
	],
};
