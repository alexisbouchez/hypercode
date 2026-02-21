import type { Lesson } from "../../types";

export const rutherfordScattering: Lesson = {
	id: "rutherford-scattering",
	title: "Rutherford Scattering",
	chapterId: "scattering",
	content: `## Rutherford Scattering

In 1911, Ernest Rutherford directed alpha particles at a thin gold foil and found that some scattered at very large angles — impossible if the atom were a diffuse "plum pudding." He deduced that the atom has a tiny, dense, positively charged **nucleus**.

### Coulomb Differential Cross Section

For a particle of charge $Z_1 e$ scattering off a nucleus of charge $Z_2 e$ via the Coulomb force, the differential cross section is:

$$\\frac{d\\sigma}{d\\Omega} = \\left(\\frac{Z_1 Z_2 \\alpha \\hbar c}{4 E_{\\text{kin}}}\\right)^2 \\frac{1}{\\sin^4(\\theta/2)}$$

The combination $\\alpha \\hbar c = 1.44 \\times 10^{-3}$ GeV·fm is the Coulomb strength parameter. With energies in GeV, the result is in **fm²/sr**.

Key features:
- Diverges as $\\theta \\to 0$ (forward scattering, long-range Coulomb force)
- Falls steeply with increasing angle ($\\sin^{-4}$)
- Scales as $Z_1^2 Z_2^2$ — heavy nuclei scatter much more strongly

### Distance of Closest Approach

For a **head-on** collision, all kinetic energy converts to Coulomb potential energy at the turning point:

$$E_{\\text{kin}} = \\frac{Z_1 Z_2 e^2}{4\\pi\\varepsilon_0 r_{\\text{min}}} \\quad \\Rightarrow \\quad r_{\\text{min}} = \\frac{Z_1 Z_2 \\alpha \\hbar c}{E_{\\text{kin}}}$$

For 5 MeV alpha particles on gold: $r_{\\text{min}} \\approx 45.5$ fm — safely outside the nuclear radius (~7 fm), confirming pure Coulomb scattering.

### Your Task

Implement two functions. All constants must be defined **inside** each function body.
- \`rutherford_dsigma_dOmega(Z1, Z2, E_kin_GeV, theta_rad)\` — result in fm²/sr
- \`closest_approach_fm(Z1, Z2, E_kin_GeV)\` — result in fm`,

	starterCode: `import math

def rutherford_dsigma_dOmega(Z1, Z2, E_kin_GeV, theta_rad):
    alpha_hbarc = 1.44e-3  # GeV·fm
    # TODO: (Z1*Z2*alpha_hbarc)^2 / (4*E_kin^2) / sin^4(theta/2)
    pass

def closest_approach_fm(Z1, Z2, E_kin_GeV):
    alpha_hbarc = 1.44e-3  # GeV·fm
    # TODO: Z1*Z2*alpha_hbarc / E_kin_GeV
    pass
`,

	solution: `import math

def rutherford_dsigma_dOmega(Z1, Z2, E_kin_GeV, theta_rad):
    alpha_hbarc = 1.44e-3  # GeV·fm (= alpha * hbar * c)
    return (Z1 * Z2 * alpha_hbarc) ** 2 / (4 * E_kin_GeV ** 2) * 1 / math.sin(theta_rad / 2) ** 4

def closest_approach_fm(Z1, Z2, E_kin_GeV):
    alpha_hbarc = 1.44e-3  # GeV·fm
    return Z1 * Z2 * alpha_hbarc / E_kin_GeV
`,

	tests: [
		{
			name: "rutherford_dsigma_dOmega(alpha on Au, 90°) ≈ 2070.61 fm²/sr",
			code: `{{FUNC}}
import math
print(round(rutherford_dsigma_dOmega(2, 79, 0.005, math.pi / 2), 2))`,
			expected: "2070.61\n",
		},
		{
			name: "rutherford_dsigma_dOmega(p+p, 30°) ≈ 115.5261 fm²/sr",
			code: `{{FUNC}}
import math
print(round(rutherford_dsigma_dOmega(1, 1, 0.001, math.pi / 6), 4))`,
			expected: "115.5261\n",
		},
		{
			name: "closest_approach_fm(alpha on Au, 5 MeV) ≈ 45.5 fm",
			code: `{{FUNC}}
print(round(closest_approach_fm(2, 79, 0.005), 2))`,
			expected: "45.5\n",
		},
		{
			name: "closest_approach_fm(p+p, 1 MeV) = 1.44 fm",
			code: `{{FUNC}}
print(round(closest_approach_fm(1, 1, 0.001), 4))`,
			expected: "1.44\n",
		},
	],
};
