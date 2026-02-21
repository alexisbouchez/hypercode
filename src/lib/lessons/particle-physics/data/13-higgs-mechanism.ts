import type { Lesson } from "../../types";

export const higgsMechanism: Lesson = {
	id: "higgs-mechanism",
	title: "Higgs Mechanism",
	chapterId: "standard-model",
	content: `## The Higgs Mechanism

The **Higgs mechanism** is the process by which the W and Z bosons — and all fundamental fermions — acquire mass without violating the gauge symmetries of the Standard Model. The discovery of the Higgs boson at CERN in 2012 (Nobel Prize 2013) confirmed this picture.

### Spontaneous Symmetry Breaking and the VEV

The Higgs field $\\phi$ acquires a non-zero **vacuum expectation value (VEV)** $v$ at the minimum of its potential:

$$V(\\phi) = -\\mu^2|\\phi|^2 + \\lambda|\\phi|^4$$

The VEV is fixed by the Fermi constant $G_F$:

$$v = \\frac{1}{\\sqrt{\\sqrt{2}\\, G_F}} \\approx 246.22 \\text{ GeV}$$

where $G_F = 1.1663788 \\times 10^{-5}$ GeV$^{-2}$.

### Gauge Boson Masses

Expanding around the VEV gives mass terms for the W and Z bosons. The W boson trilinear coupling to the Higgs is:

$$g_{HWW} = \\frac{2 m_W^2}{v}$$

Similarly for the Z: $g_{HZZ} = 2m_Z^2/v$.

### Yukawa Couplings and Fermion Masses

Fermions couple to the Higgs field through **Yukawa interactions**:

$$\\mathcal{L}_Y = -y_f \\bar{\\psi}_f \\phi \\psi_f$$

After symmetry breaking, the fermion mass is $m_f = y_f v / \\sqrt{2}$, so:

$$y_f = \\frac{\\sqrt{2}\\, m_f}{v}$$

The **top quark** ($m_t \\approx 173$ GeV) has $y_t \\approx 1$ — it is nearly as strongly coupled to the Higgs as theoretically natural. The **electron** ($m_e \\approx 0.511$ MeV) has $y_e \\approx 3 \\times 10^{-6}$, an unexplained hierarchy.

### Your Task

Implement:
- \`higgs_vev()\` — the Higgs VEV in GeV from $G_F$
- \`yukawa_coupling(m_fermion_GeV)\` — Yukawa coupling $y_f = \\sqrt{2}\\,m_f/v$ (compute $v$ inline)
- \`higgs_to_WW_coupling(m_W_GeV)\` — trilinear Higgs–WW coupling $2m_W^2/v$ (compute $v$ inline)

All constants must be defined **inside** each function body.`,

	starterCode: `import math

def higgs_vev():
    # G_F = 1.1663788e-5 GeV^-2
    # v = 1 / sqrt(sqrt(2) * G_F)
    pass

def yukawa_coupling(m_fermion_GeV):
    # y_f = sqrt(2) * m_f / v
    # compute v inline using G_F = 1.1663788e-5
    pass

def higgs_to_WW_coupling(m_W_GeV):
    # g_HWW = 2 * m_W^2 / v
    # compute v inline using G_F = 1.1663788e-5
    pass
`,

	solution: `import math

def higgs_vev():
    G_F = 1.1663788e-5
    return 1 / math.sqrt(math.sqrt(2) * G_F)

def yukawa_coupling(m_fermion_GeV):
    G_F = 1.1663788e-5
    v = 1 / math.sqrt(math.sqrt(2) * G_F)
    return math.sqrt(2) * m_fermion_GeV / v

def higgs_to_WW_coupling(m_W_GeV):
    G_F = 1.1663788e-5
    v = 1 / math.sqrt(math.sqrt(2) * G_F)
    return 2 * m_W_GeV ** 2 / v
`,

	tests: [
		{
			name: "higgs_vev() ≈ 246.22 GeV",
			code: `{{FUNC}}
print(round(higgs_vev(), 2))`,
			expected: "246.22\n",
		},
		{
			name: "yukawa_coupling(173.076) ≈ 0.994098 (top quark, near unity)",
			code: `{{FUNC}}
print(round(yukawa_coupling(173.076), 6))`,
			expected: "0.994098\n",
		},
		{
			name: "yukawa_coupling(0.000511) ≈ 2.94e-06 (electron, tiny Yukawa)",
			code: `{{FUNC}}
print(round(yukawa_coupling(0.000511), 8))`,
			expected: "2.94e-06\n",
		},
		{
			name: "higgs_to_WW_coupling(80.377) ≈ 52.4772 GeV (trilinear HWW coupling)",
			code: `{{FUNC}}
print(round(higgs_to_WW_coupling(80.377), 4))`,
			expected: "52.4772\n",
		},
	],
};
