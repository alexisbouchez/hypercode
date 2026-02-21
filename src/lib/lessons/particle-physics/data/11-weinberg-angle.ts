import type { Lesson } from "../../types";

export const weinbergAngle: Lesson = {
	id: "weinberg-angle",
	title: "Weinberg Angle and Electroweak Unification",
	chapterId: "standard-model",
	content: `## Weinberg Angle and Electroweak Unification

The **electroweak theory** unifies electromagnetism and the weak nuclear force into a single framework. The key parameter that links the two interactions is the **Weinberg angle** (also called the **weak mixing angle**) $\\theta_W$.

### The Weinberg Angle

The measured value at the Z boson mass scale is:

$$\\sin^2\\theta_W \\approx 0.23122 \\quad (Q = m_Z)$$

This angle "mixes" the $SU(2)_L$ and $U(1)_Y$ gauge bosons to produce the photon and the Z boson. It relates the electromagnetic coupling $e$, the weak coupling $g$, and the hypercharge coupling $g'$:

$$g \\sin\\theta_W = g' \\cos\\theta_W = e$$

### W and Z Boson Masses

At tree level, the electroweak theory predicts a precise relationship between the W and Z boson masses:

$$m_W = m_Z \\cos\\theta_W = m_Z \\sqrt{1 - \\sin^2\\theta_W}$$

With $m_Z = 91.1876$ GeV and $\\sin^2\\theta_W = 0.23122$:

$$m_W = 91.1876 \\times \\sqrt{1 - 0.23122} \\approx 79.95 \\text{ GeV}$$

The measured value is $m_W \\approx 80.377$ GeV — deviations are explained by radiative corrections.

### Tree-Level W Mass from the Fermi Constant

The **Fermi constant** $G_F = 1.1663788 \\times 10^{-5}$ GeV$^{-2}$ governs the strength of weak decays (like muon decay). At tree level, the W mass satisfies:

$$m_W^2 \\sin^2\\theta_W = \\frac{\\pi \\alpha}{\\sqrt{2} \\, G_F}$$

$$m_W = \\frac{1}{\\sin\\theta_W} \\sqrt{\\frac{\\pi \\alpha}{\\sqrt{2} \\, G_F}}$$

where $\\alpha \\approx 1/137.036$ is the fine-structure constant. This gives a slightly different value than the $m_Z \\cos\\theta_W$ relation because loop corrections enter differently.

### Your Task

Implement three functions:
- \`w_mass_from_z(m_Z_GeV, sin2_theta_W)\` — W mass from the Z mass using $m_W = m_Z\\sqrt{1-\\sin^2\\theta_W}$
- \`weinberg_angle_deg()\` — the Weinberg angle in degrees from $\\sin^2\\theta_W = 0.23122$
- \`w_mass_from_fermi(alpha, G_F_GeV2)\` — tree-level W mass from the Fermi constant

All physics constants must be defined **inside** each function body.`,

	starterCode: `import math

def w_mass_from_z(m_Z_GeV, sin2_theta_W):
    # m_W = m_Z * cos(theta_W) = m_Z * sqrt(1 - sin2_theta_W)
    pass

def weinberg_angle_deg():
    # sin2_theta_W = 0.23122
    # return theta_W in degrees
    pass

def w_mass_from_fermi(alpha, G_F_GeV2):
    # Tree-level: m_W = sqrt(pi*alpha / (sqrt(2)*G_F)) / sqrt(sin2_theta_W)
    # sin2_theta_W = 0.23122
    pass
`,

	solution: `import math

def w_mass_from_z(m_Z_GeV, sin2_theta_W):
    return m_Z_GeV * math.sqrt(1 - sin2_theta_W)

def weinberg_angle_deg():
    sin2_theta_W = 0.23122
    return math.degrees(math.asin(math.sqrt(sin2_theta_W)))

def w_mass_from_fermi(alpha, G_F_GeV2):
    sin2_theta_W = 0.23122
    return math.sqrt(math.pi * alpha / (math.sqrt(2) * G_F_GeV2)) / math.sqrt(sin2_theta_W)
`,

	tests: [
		{
			name: "w_mass_from_z(91.1876, 0.23122) ≈ 79.9534 GeV",
			code: `{{FUNC}}
print(round(w_mass_from_z(91.1876, 0.23122), 4))`,
			expected: "79.9534\n",
		},
		{
			name: "weinberg_angle_deg() ≈ 28.7412°",
			code: `{{FUNC}}
print(round(weinberg_angle_deg(), 4))`,
			expected: "28.7412\n",
		},
		{
			name: "w_mass_from_fermi(1/137.036, 1.1663788e-5) ≈ 77.5296 GeV (tree level)",
			code: `{{FUNC}}
print(round(w_mass_from_fermi(1/137.036, 1.1663788e-5), 4))`,
			expected: "77.5296\n",
		},
		{
			name: "sin²θ_W self-consistency: 1 - (m_W/m_Z)² = 0.23122",
			code: `{{FUNC}}
val = 1 - (w_mass_from_z(91.1876, 0.23122) / 91.1876) ** 2
print(round(val, 5))`,
			expected: "0.23122\n",
		},
	],
};
