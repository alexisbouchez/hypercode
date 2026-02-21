import type { Lesson } from "../../types";

export const particleLifetimes: Lesson = {
	id: "particle-lifetimes",
	title: "Particle Lifetimes and the Standard Model",
	chapterId: "decays",
	content: `## Particle Lifetimes and the Standard Model

Particle lifetimes in the Standard Model span more than **40 orders of magnitude** — from the proton's stability over cosmic timescales to the fleeting existence of the W boson. Understanding what sets these lifetimes reveals deep connections between symmetries, coupling strengths, and phase space.

### The Width–Lifetime Relation

Every unstable particle has a **decay width** $\\Gamma$ (in energy units) related to its lifetime $\\tau$ by:

$$\\Gamma \\cdot \\tau = \\hbar = 6.582119569 \\times 10^{-25} \\text{ GeV·s}$$

A broad resonance ($\\Gamma$ large) decays quickly ($\\tau$ small). The **natural linewidth** of any state is just $\\Gamma = \\hbar/\\tau$.

### Lifetime Hierarchy

| Particle | Lifetime $\\tau$ | Dominant decay |
|----------|----------------|---------------|
| Proton | $> 10^{34}$ yr | Stable (baryon number) |
| Neutron | 878.4 s | $\\beta$ decay ($n \\to p e^- \\bar\\nu_e$) |
| Muon $\\mu^\\pm$ | $2.197 \\times 10^{-6}$ s | Weak |
| Pion $\\pi^\\pm$ | $2.603 \\times 10^{-8}$ s | Weak ($\\pi^+ \\to \\mu^+\\nu_\\mu$) |
| Pion $\\pi^0$ | $8.52 \\times 10^{-17}$ s | EM ($\\pi^0 \\to \\gamma\\gamma$) |
| B meson | $1.638 \\times 10^{-12}$ s | Weak (CKM mixing) |
| W boson | $\\Gamma = 2.085$ GeV | $W \\to \\ell\\nu, q\\bar{q}'$ |
| Higgs boson | $\\Gamma = 4.07 \\times 10^{-3}$ GeV | $H \\to b\\bar{b}, WW^*, ZZ^*, \\ldots$ |

### Relativistic Decay Length

A particle produced with momentum $p$ and mass $m$ travels (on average) a distance before decaying:

$$L = \\frac{p}{m}\\, c\\, \\tau = \\beta\\gamma c\\tau$$

This is crucial for detector design. A charged pion with $p = 100$ GeV travels **kilometers** before decaying — it easily reaches the muon detectors. A B meson with $p \\sim 50$ GeV travels only **~mm**, requiring precision vertex detectors (silicon pixels) close to the interaction point.

### Your Task

Implement:
- \`natural_width(tau_s)\` — decay width $\\Gamma = \\hbar/\\tau$ in GeV
- \`decay_length_at_LHC(mass_GeV, tau_s, momentum_GeV)\` — relativistic decay length in metres
- \`compare_widths(Gamma1_GeV, Gamma2_GeV)\` — ratio of two decay widths

All constants must be defined **inside** each function body.`,

	starterCode: `import math

def natural_width(tau_s):
    # hbar = 6.582119569e-25 GeV*s
    # return hbar / tau_s  [GeV]
    pass

def decay_length_at_LHC(mass_GeV, tau_s, momentum_GeV):
    # L = (p/m) * c * tau  [m]
    # c = 2.998e8 m/s
    pass

def compare_widths(Gamma1_GeV, Gamma2_GeV):
    # return ratio Gamma1 / Gamma2
    pass
`,

	solution: `import math

def natural_width(tau_s):
    hbar = 6.582119569e-25
    return hbar / tau_s

def decay_length_at_LHC(mass_GeV, tau_s, momentum_GeV):
    c = 2.998e8
    return (momentum_GeV / mass_GeV) * c * tau_s

def compare_widths(Gamma1_GeV, Gamma2_GeV):
    return Gamma1_GeV / Gamma2_GeV
`,

	tests: [
		{
			name: "natural_width(2.197e-6) ≈ 2.9960e-19 GeV (muon natural linewidth)",
			code: `{{FUNC}}
print(f"{natural_width(2.197e-6):.4e}")`,
			expected: "2.9960e-19\n",
		},
		{
			name: "natural_width(8.52e-17) ≈ 7.73e-09 GeV (π⁰ width, EM decay)",
			code: `{{FUNC}}
print(f"{natural_width(8.52e-17):.2e}")`,
			expected: "7.73e-09\n",
		},
		{
			name: "decay_length_at_LHC(0.13957, 2.603e-8, 100.0) ≈ 5591.3 m (π± at 100 GeV)",
			code: `{{FUNC}}
print(round(decay_length_at_LHC(0.13957, 2.603e-8, 100.0), 1))`,
			expected: "5591.3\n",
		},
		{
			name: "compare_widths(2.4952, 4.07e-3) ≈ 613.07 (Z width / Higgs width)",
			code: `{{FUNC}}
print(round(compare_widths(2.4952, 4.07e-3), 2))`,
			expected: "613.07\n",
		},
	],
};
