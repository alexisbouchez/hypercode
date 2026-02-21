import type { Lesson } from "../../types";

export const labFrameBoost: Lesson = {
	id: "lab-frame-boost",
	title: "Lab Frame Decay Length",
	chapterId: "decays",
	content: `## Lab Frame Decay Length

A particle produced in a collider is not at rest — it flies through the detector at high speed. This means its proper lifetime $\\tau_0$ is dilated in the lab frame by the Lorentz factor $\\gamma$.

### Kinematics in Natural Units

In high-energy physics we work in natural units where $c = 1$. For a particle with energy $E$ and mass $m$:

$$\\gamma = \\frac{E}{m}$$

$$\\beta = \\frac{p}{E} = \\sqrt{1 - \\frac{m^2}{E^2}}$$

The **lab-frame lifetime** is stretched by time dilation:

$$\\tau_{\\text{lab}} = \\gamma \\, \\tau_0 = \\frac{E}{m} \\, \\tau_0$$

### Mean Decay Length

The average distance a particle travels before decaying is:

$$L = \\gamma \\beta c \\tau_0 = \\frac{p}{m} \\cdot c \\cdot \\tau_0$$

In SI-friendly mixed units, with energy/mass in GeV and $c = 2.998 \\times 10^8$ m/s:

$$L \;[\\text{m}] = \\frac{E_{\\text{GeV}}}{m_{\\text{GeV}}} \\cdot \\sqrt{1 - \\left(\\frac{m}{E}\\right)^2} \\cdot c \\cdot \\tau_0$$

### Example: Muons

A muon has mass $m_\\mu = 0.10566$ GeV and proper lifetime $\\tau_\\mu = 2.197 \\times 10^{-6}$ s. At $E = 1$ GeV, $\\gamma \\approx 9.46$, and the mean decay length is over **6 km** — explaining why cosmic-ray muons survive from the upper atmosphere to sea level.

### Your Task

Implement three functions. All constants must be defined **inside** each function body.
- \`lorentz_factor(E_GeV, m_GeV)\` — returns $E/m$
- \`beta(E_GeV, m_GeV)\` — returns $\\sqrt{1-(m/E)^2}$
- \`mean_decay_length(E_GeV, m_GeV, tau_s)\` — returns $L$ in metres`,

	starterCode: `import math

def lorentz_factor(E_GeV, m_GeV):
    # TODO: return E / m
    pass

def beta(E_GeV, m_GeV):
    # TODO: return sqrt(1 - (m/E)^2)
    pass

def mean_decay_length(E_GeV, m_GeV, tau_s):
    c = 2.998e8  # m/s
    # TODO: return gamma * beta * c * tau_s
    pass
`,

	solution: `import math

def lorentz_factor(E_GeV, m_GeV):
    return E_GeV / m_GeV

def beta(E_GeV, m_GeV):
    return math.sqrt(1 - (m_GeV / E_GeV) ** 2)

def mean_decay_length(E_GeV, m_GeV, tau_s):
    c = 2.998e8  # m/s
    gamma = E_GeV / m_GeV
    b = math.sqrt(1 - (m_GeV / E_GeV) ** 2)
    return gamma * b * c * tau_s
`,

	tests: [
		{
			name: "lorentz_factor(1.0, 0.10566) ≈ 9.4643 (muon at 1 GeV)",
			code: `{{FUNC}}
print(round(lorentz_factor(1.0, 0.10566), 4))`,
			expected: "9.4643\n",
		},
		{
			name: "beta(1.0, 0.10566) ≈ 0.994402 (muon nearly at c)",
			code: `{{FUNC}}
print(round(beta(1.0, 0.10566), 6))`,
			expected: "0.994402\n",
		},
		{
			name: "mean_decay_length muon at 1 GeV ≈ 6198.8797 m (~6 km)",
			code: `{{FUNC}}
print(round(mean_decay_length(1.0, 0.10566, 2.197e-6), 4))`,
			expected: "6198.8797\n",
		},
		{
			name: "mean_decay_length pion at 10 GeV ≈ 558.43 m",
			code: `{{FUNC}}
print(round(mean_decay_length(10.0, 0.13957, 2.6e-8), 2))`,
			expected: "558.43\n",
		},
	],
};
