import type { Lesson } from "../../types";

export const fineStructure: Lesson = {
	id: "fine-structure",
	title: "Fine Structure Constant and QED",
	chapterId: "scattering",
	content: `## Fine Structure Constant and QED

The **fine structure constant** $\\alpha$ is the dimensionless coupling constant of quantum electrodynamics (QED). It sets the strength of all electromagnetic interactions:

$$\\alpha = \\frac{e^2}{4\\pi\\varepsilon_0 \\hbar c} \\approx \\frac{1}{137.036} \\approx 7.297 \\times 10^{-3}$$

Its smallness ($\\alpha \\ll 1$) is why perturbation theory works so well in QED — higher-order Feynman diagrams are suppressed by additional powers of $\\alpha$.

### Running Coupling

In quantum field theory, coupling constants **run** with the energy scale $Q$. At one loop in QED:

$$\\alpha(Q^2) \\approx \\frac{\\alpha(0)}{1 - \\dfrac{\\alpha(0)}{3\\pi} \\ln\\!\\left(\\dfrac{Q^2}{m_e^2}\\right)}$$

where $m_e = 0.000511$ GeV is the electron mass. The coupling **increases** at higher energies because virtual electron-positron pairs screen the bare charge less effectively at short distances.

At the $Z$ pole ($Q = M_Z \\approx 91.2$ GeV):

$$\\alpha(M_Z^2) \\approx \\frac{1}{128.9} \\approx 0.00776$$

compared to $\\alpha(0) \\approx 1/137.036$ at zero energy.

### Cross Section Scaling

QED amplitudes are proportional to $\\alpha$, so cross sections scale as $\\alpha^2$. The ratio of cross sections at two different energies is:

$$\\frac{\\sigma(Q_1)}{\\sigma(Q_2)} = \\left(\\frac{\\alpha(Q_1)}{\\alpha(Q_2)}\\right)^2$$

### Your Task

Implement three functions. All constants must be defined **inside** each function body.
- \`alpha_QED()\` — returns $\\alpha$ at $Q = 0$
- \`alpha_running(Q_GeV)\` — one-loop running coupling
- \`qed_cross_section_ratio(Q1_GeV, Q2_GeV)\` — ratio $(\\alpha(Q_1)/\\alpha(Q_2))^2$`,

	starterCode: `import math

def alpha_QED():
    # TODO: return 1 / 137.035999084
    pass

def alpha_running(Q_GeV):
    a0 = 1 / 137.035999084
    me = 0.000511  # GeV
    # TODO: one-loop: a0 / (1 - (a0/(3*pi)) * log(Q^2/me^2))
    pass

def qed_cross_section_ratio(Q1_GeV, Q2_GeV):
    # TODO: (alpha_running(Q1) / alpha_running(Q2))**2
    pass
`,

	solution: `import math

def alpha_QED():
    return 1 / 137.035999084

def alpha_running(Q_GeV):
    a0 = 1 / 137.035999084
    me = 0.000511  # GeV
    return a0 / (1 - (a0 / (3 * math.pi)) * math.log(Q_GeV ** 2 / me ** 2))

def qed_cross_section_ratio(Q1_GeV, Q2_GeV):
    a0 = 1 / 137.035999084
    me = 0.000511
    def a(Q):
        return a0 / (1 - (a0 / (3 * math.pi)) * math.log(Q ** 2 / me ** 2))
    return (a(Q1_GeV) / a(Q2_GeV)) ** 2
`,

	tests: [
		{
			name: "alpha_QED() ≈ 0.00729735 (1/137.036)",
			code: `{{FUNC}}
print(round(alpha_QED(), 8))`,
			expected: "0.00729735\n",
		},
		{
			name: "alpha_running(91.1876) ≈ 0.007437 (1/134.5 at Z pole)",
			code: `{{FUNC}}
print(round(alpha_running(91.1876), 6))`,
			expected: "0.007437\n",
		},
		{
			name: "alpha_running(0.001) ≈ 0.00730495 (near low-energy value)",
			code: `{{FUNC}}
print(round(alpha_running(0.001), 8))`,
			expected: "0.00730495\n",
		},
		{
			name: "1/alpha_running(91.1876) ≈ 134.47",
			code: `{{FUNC}}
print(round(1 / alpha_running(91.1876), 2))`,
			expected: "134.47\n",
		},
	],
};
