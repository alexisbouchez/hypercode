import type { Lesson } from "../../types";

export const twoBodyDecay: Lesson = {
	id: "two-body-decay",
	title: "Two-Body Decay Kinematics",
	chapterId: "decays",
	content: `## Two-Body Decay Kinematics

When a particle $A$ of mass $M$ decays at rest into two daughters $B$ and $C$, conservation of 4-momentum uniquely fixes the kinematics. Working in natural units ($c = 1$, all quantities in GeV):

### Momentum of the Daughters

Both daughters recoil back-to-back with equal and opposite momenta of magnitude $p^*$:

$$p^* = \\frac{\\sqrt{\\bigl[M^2 - (m_B + m_C)^2\\bigr]\\bigl[M^2 - (m_B - m_C)^2\\bigr]}}{2M}$$

This is real only when $M \\geq m_B + m_C$ (energy conservation requires the parent to be heavier than the sum of daughters).

### Energies of the Daughters

$$E_B^* = \\frac{M^2 + m_B^2 - m_C^2}{2M}, \\qquad E_C^* = \\frac{M^2 + m_C^2 - m_B^2}{2M}$$

Note that $E_B^* + E_C^* = M$ (total energy equals parent rest mass) and $E_B^{*2} - p^{*2} = m_B^2$ as required.

### Example: $\\pi^0 \\to \\gamma\\gamma$

With $M = 0.135$ GeV and $m_B = m_C = 0$ (massless photons):

$$p^* = E_B^* = E_C^* = \\frac{M}{2} = 0.0675 \\text{ GeV}$$

### Your Task

Implement (all masses and energies in GeV, natural units):

- \`decay_momentum(M_GeV, mB_GeV, mC_GeV)\` — returns $p^*$
- \`decay_energy_B(M_GeV, mB_GeV, mC_GeV)\` — returns $E_B^*$
- \`decay_energy_C(M_GeV, mB_GeV, mC_GeV)\` — returns $E_C^*$`,

	starterCode: `import math

def decay_momentum(M_GeV, mB_GeV, mC_GeV):
    # p* = sqrt(max(0, (M^2-(mB+mC)^2)*(M^2-(mB-mC)^2))) / (2*M)
    pass

def decay_energy_B(M_GeV, mB_GeV, mC_GeV):
    # EB* = (M^2 + mB^2 - mC^2) / (2*M)
    pass

def decay_energy_C(M_GeV, mB_GeV, mC_GeV):
    # EC* = (M^2 + mC^2 - mB^2) / (2*M)
    pass

print(round(decay_momentum(0.135, 0.0, 0.0), 4))
print(round(decay_momentum(91.1876, 0.000511, 0.000511), 4))
print(round(decay_energy_B(80.377, 0.000511, 0.0), 4))
print(round(decay_momentum(0.497611, 0.13957, 0.13957), 6))
`,

	solution: `import math

def decay_momentum(M_GeV, mB_GeV, mC_GeV):
    factor = (M_GeV**2 - (mB_GeV + mC_GeV)**2) * (M_GeV**2 - (mB_GeV - mC_GeV)**2)
    return math.sqrt(max(0, factor)) / (2*M_GeV)

def decay_energy_B(M_GeV, mB_GeV, mC_GeV):
    return (M_GeV**2 + mB_GeV**2 - mC_GeV**2) / (2*M_GeV)

def decay_energy_C(M_GeV, mB_GeV, mC_GeV):
    return (M_GeV**2 + mC_GeV**2 - mB_GeV**2) / (2*M_GeV)

print(round(decay_momentum(0.135, 0.0, 0.0), 4))
print(round(decay_momentum(91.1876, 0.000511, 0.000511), 4))
print(round(decay_energy_B(80.377, 0.000511, 0.0), 4))
print(round(decay_momentum(0.497611, 0.13957, 0.13957), 6))
`,

	tests: [
		{
			name: "π⁰ → γγ: decay_momentum(0.135, 0, 0) = 0.0675 GeV (half pion mass)",
			code: `{{FUNC}}
print(round(decay_momentum(0.135, 0.0, 0.0), 4))`,
			expected: "0.0675\n",
		},
		{
			name: "Z → e⁺e⁻: decay_momentum(91.1876, 0.000511, 0.000511) ≈ 45.5938 GeV",
			code: `{{FUNC}}
print(round(decay_momentum(91.1876, 0.000511, 0.000511), 4))`,
			expected: "45.5938\n",
		},
		{
			name: "W → eνe: decay_energy_B(80.377, 0.000511, 0.0) ≈ 40.1885 GeV",
			code: `{{FUNC}}
print(round(decay_energy_B(80.377, 0.000511, 0.0), 4))`,
			expected: "40.1885\n",
		},
		{
			name: "K⁰ → π⁺π⁻: decay_momentum(0.497611, 0.13957, 0.13957) ≈ 0.205972 GeV",
			code: `{{FUNC}}
print(round(decay_momentum(0.497611, 0.13957, 0.13957), 6))`,
			expected: "0.205972\n",
		},
	],
};
