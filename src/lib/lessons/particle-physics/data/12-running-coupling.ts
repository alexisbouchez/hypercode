import type { Lesson } from "../../types";

export const runningCoupling: Lesson = {
	id: "running-coupling",
	title: "Running Coupling Constants",
	chapterId: "standard-model",
	content: `## Running Coupling Constants

One of the most profound results of Quantum Field Theory is that coupling constants are not truly constant — they **run** with the energy scale $Q$ at which they are probed. This running arises from quantum corrections (loop diagrams) that dress the bare interaction vertex.

### QED: α Increases with Energy

In Quantum Electrodynamics the fine-structure constant $\\alpha$ grows logarithmically with energy due to vacuum polarisation (electron–positron virtual pairs screen the bare charge):

$$\\alpha(Q) = \\frac{\\alpha_0}{1 - \\dfrac{\\alpha_0}{3\\pi} \\ln\\!\\left(\\dfrac{Q^2}{m_e^2}\\right)}$$

where $\\alpha_0 = 1/137.036$ is the low-energy value and $m_e = 5.11 \\times 10^{-4}$ GeV. At the Z mass, $\\alpha(m_Z) \\approx 1/134$.

### QCD: αs Decreases with Energy (Asymptotic Freedom)

The strong coupling constant $\\alpha_s$ does the opposite: it **decreases** at high energy. This is **asymptotic freedom**, discovered by Gross, Politzer, and Wilczek (Nobel Prize 2004). At one loop:

$$\\alpha_s(Q) = \\frac{12\\pi}{(33 - 2n_f) \\ln(Q^2 / \\Lambda_{\\text{QCD}}^2)}$$

where $n_f$ is the number of active quark flavours (typically 5 above the bottom threshold), and $\\Lambda_{\\text{QCD}} \\approx 0.217$ GeV is the QCD scale where the coupling diverges. The coefficient $\\beta_0 = 11 - 2n_f/3 = (33 - 2n_f)/6$ governs the running speed.

| Scale $Q$ | $\\alpha_s$ |
|-----------|-----------|
| 1 GeV | $\\approx 0.5$ (non-perturbative) |
| $m_Z = 91$ GeV | $0.1179$ (measured) |
| 1 TeV | $\\approx 0.09$ |

### Your Task

Implement:
- \`alpha_QED_running(Q_GeV)\` — running QED coupling at scale $Q$
- \`alpha_s_running(Q_GeV, n_f=5, Lambda_QCD=0.217)\` — one-loop QCD running coupling
- \`coupling_ratio_QCD_QED(Q_GeV)\` — ratio $\\alpha_s / \\alpha$ at scale $Q$ (implement both inline)

All constants must be defined **inside** each function body.`,

	starterCode: `import math

def alpha_QED_running(Q_GeV):
    # a0 = 1/137.035999084, me = 5.11e-4 GeV
    # return a0 / (1 - (a0/(3*pi)) * ln(Q^2/me^2))
    pass

def alpha_s_running(Q_GeV, n_f=5, Lambda_QCD=0.217):
    # return 12*pi / ((33 - 2*n_f) * ln(Q^2/Lambda^2))
    pass

def coupling_ratio_QCD_QED(Q_GeV):
    # Compute both alpha_s and alpha_QED inline, return ratio
    pass
`,

	solution: `import math

def alpha_QED_running(Q_GeV):
    a0 = 1/137.035999084
    me = 5.11e-4
    return a0 / (1 - (a0/(3*math.pi))*math.log(Q_GeV**2/me**2))

def alpha_s_running(Q_GeV, n_f=5, Lambda_QCD=0.217):
    return 12*math.pi / ((33 - 2*n_f)*math.log(Q_GeV**2/Lambda_QCD**2))

def coupling_ratio_QCD_QED(Q_GeV):
    a0 = 1/137.035999084
    me = 5.11e-4
    alpha_qed = a0 / (1 - (a0/(3*math.pi))*math.log(Q_GeV**2/me**2))
    n_f = 5
    Lambda_QCD = 0.217
    alpha_s = 12*math.pi / ((33 - 2*n_f)*math.log(Q_GeV**2/Lambda_QCD**2))
    return alpha_s / alpha_qed
`,

	tests: [
		{
			name: "alpha_QED_running(91.1876) ≈ 0.007437 (α grows at high energy)",
			code: `{{FUNC}}
print(round(alpha_QED_running(91.1876), 6))`,
			expected: "0.007437\n",
		},
		{
			name: "alpha_s_running(91.1876) ≈ 0.1357 (one-loop QCD at Z mass)",
			code: `{{FUNC}}
print(round(alpha_s_running(91.1876), 4))`,
			expected: "0.1357\n",
		},
		{
			name: "alpha_s_running(1000.0) < alpha_s_running(91.1876) (asymptotic freedom)",
			code: `{{FUNC}}
print(round(alpha_s_running(1000.0), 4))`,
			expected: "0.0972\n",
		},
		{
			name: "alpha_s_running(1.0) ≈ 0.5364 (strong coupling at 1 GeV)",
			code: `{{FUNC}}
print(round(alpha_s_running(1.0), 4))`,
			expected: "0.5364\n",
		},
	],
};
