import type { Lesson } from "../../types";

export const breitWigner: Lesson = {
	id: "breit-wigner",
	title: "Breit-Wigner Resonance",
	chapterId: "scattering",
	content: `## Breit-Wigner Resonance

When the centre-of-mass energy $\\sqrt{s}$ passes through a particle's rest mass $M$, the cross section peaks sharply. This **resonance** is described by the non-relativistic Breit-Wigner formula:

$$\\sigma(E) = \\frac{\\sigma_{\\text{peak}}}{1 + \\left(\\dfrac{2(E - M)}{\\Gamma}\\right)^2}$$

where $\\Gamma$ is the **total decay width** (inverse lifetime). The shape is a Lorentzian centred at $E = M$ with full width at half maximum equal to $\\Gamma$.

### Peak Cross Section for Z Production

For $e^+e^- \\to Z \\to$ hadrons, the peak cross section is:

$$\\sigma_{\\text{peak}} = \\frac{12\\pi}{M_Z^2} \\cdot \\frac{\\Gamma_{ee} \\Gamma_{\\text{had}}}{\\Gamma_Z^2}$$

In natural units this has dimensions of GeV$^{-2}$. Using the conversion $1 \\text{ GeV}^{-2} = 3.894 \\times 10^5 \\text{ nb}$:

$$\\sigma_{\\text{peak}} \\approx 41.4 \\text{ nb}$$

This enormous cross section (compared to typical QED backgrounds $\\sim 1$ nb) made the $Z$ lineshape measurement at LEP exquisitely precise.

### Parameters used (PDG values)

| Parameter | Value |
|-----------|-------|
| $M_Z$ | 91.1876 GeV |
| $\\Gamma_Z$ | 2.4952 GeV |
| $\\Gamma_{ee}$ | 0.08391 GeV |
| $\\Gamma_{\\text{had}}$ | 1.7408 GeV |

### Measuring $\\Gamma$ from the Lineshape

The half-maximum points of the resonance occur at $E = M \\pm \\Gamma/2$. The full width $\\Gamma = E_2 - E_1$ can be read directly from the energy scan.

### Your Task

Implement three functions. All constants must be defined **inside** each function body.
- \`breit_wigner(E_GeV, M_GeV, Gamma_GeV)\` — normalised Breit-Wigner (peak = 1)
- \`z_peak_cross_section_nb()\` — peak cross section for $e^+e^- \\to Z \\to$ hadrons in nb
- \`width_from_peak_shape(E1_GeV, E2_GeV, M_GeV)\` — extract $\\Gamma$ from half-max energies`,

	starterCode: `import math

def breit_wigner(E_GeV, M_GeV, Gamma_GeV):
    # TODO: 1 / (1 + (2*(E-M)/Gamma)^2)
    pass

def z_peak_cross_section_nb():
    M_Z = 91.1876
    Gamma_Z = 2.4952
    Gamma_ee = 0.08391
    Gamma_had = 1.7408
    # TODO: 12*pi/M_Z^2 * Gamma_ee*Gamma_had/Gamma_Z^2, then convert to nb
    pass

def width_from_peak_shape(E1_GeV, E2_GeV, M_GeV):
    # TODO: return E2 - E1
    pass
`,

	solution: `import math

def breit_wigner(E_GeV, M_GeV, Gamma_GeV):
    return 1.0 / (1 + (2 * (E_GeV - M_GeV) / Gamma_GeV) ** 2)

def z_peak_cross_section_nb():
    M_Z = 91.1876
    Gamma_Z = 2.4952
    Gamma_ee = 0.08391
    Gamma_had = 1.7408
    conv = 3.894e5  # 1 GeV^-2 = 3.894e5 nb
    sigma_GeV2 = 12 * math.pi / M_Z ** 2 * Gamma_ee * Gamma_had / Gamma_Z ** 2
    return sigma_GeV2 * conv

def width_from_peak_shape(E1_GeV, E2_GeV, M_GeV):
    return E2_GeV - E1_GeV
`,

	tests: [
		{
			name: "breit_wigner at peak = 1.0",
			code: `{{FUNC}}
print(breit_wigner(91.1876, 91.1876, 2.4952))`,
			expected: "1.0\n",
		},
		{
			name: "breit_wigner at half-maximum point = 0.5",
			code: `{{FUNC}}
print(round(breit_wigner(91.1876 + 2.4952 / 2, 91.1876, 2.4952), 4))`,
			expected: "0.5\n",
		},
		{
			name: "z_peak_cross_section_nb() ≈ 41.42 nb",
			code: `{{FUNC}}
print(round(z_peak_cross_section_nb(), 2))`,
			expected: "41.42\n",
		},
		{
			name: "breit_wigner(91.0, 91.1876, 2.4952) ≈ 0.977889",
			code: `{{FUNC}}
print(round(breit_wigner(91.0, 91.1876, 2.4952), 6))`,
			expected: "0.977889\n",
		},
	],
};
