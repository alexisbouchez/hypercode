import type { Lesson } from "../../types";

export const decayWidth: Lesson = {
	id: "decay-width",
	title: "Decay Width and Lifetime",
	chapterId: "decays",
	content: `## Decay Width and Lifetime

Unstable particles decay with a characteristic **mean lifetime** $\\tau$. Quantum field theory predicts the **decay width** $\\Gamma$, which is the total transition rate summed over all decay modes. They are related by:

$$\\tau = \\frac{\\hbar}{\\Gamma}$$

where $\\hbar = 6.582119569 \\times 10^{-25}$ GeV·s.

A broader resonance ($\\Gamma$ large) lives for a shorter time. The Z boson has $\\Gamma_Z = 2.4952$ GeV and lives only $\\sim 3 \\times 10^{-25}$ s, while the muon has $\\Gamma_\\mu \\sim 3 \\times 10^{-19}$ GeV and lives $2.2\\,\\mu$s.

### Exponential Decay

The number of particles surviving at time $t$:

$$N(t) = N_0\\, e^{-t/\\tau}$$

### Lab Decay Length

A relativistic particle with Lorentz factor $\\gamma$ and velocity $\\beta c$ travels a mean distance before decaying:

$$L_{\\text{lab}} = \\gamma \\beta c \\tau$$

This is why high-energy pions travel farther before decaying — time dilation extends their laboratory lifetime by $\\gamma$.

### Your Task

Implement (using $\\hbar = 6.582119569 \\times 10^{-25}$ GeV·s and $c = 2.998 \\times 10^8$ m/s, defined inside each function):

- \`lifetime_from_width(Gamma_GeV)\` — returns $\\tau = \\hbar / \\Gamma$ in seconds
- \`width_from_lifetime(tau_s)\` — returns $\\Gamma = \\hbar / \\tau$ in GeV
- \`lab_decay_length(tau_s, gamma, beta)\` — returns $L = \\gamma \\beta c \\tau$ in metres`,

	starterCode: `import math

def lifetime_from_width(Gamma_GeV):
    hbar = 6.582119569e-25  # GeV·s
    # tau = hbar / Gamma
    pass

def width_from_lifetime(tau_s):
    hbar = 6.582119569e-25  # GeV·s
    # Gamma = hbar / tau
    pass

def lab_decay_length(tau_s, gamma, beta):
    c = 2.998e8  # m/s
    # L = gamma * beta * c * tau
    pass

print(f"{lifetime_from_width(2.4952):.4e}")
print(f"{width_from_lifetime(2.197e-6):.4e}")
print(round(lab_decay_length(2.6e-8, 10, 1.0), 2))
print(f"{lifetime_from_width(4.07e-3):.4e}")
`,

	solution: `import math

def lifetime_from_width(Gamma_GeV):
    hbar = 6.582119569e-25
    return hbar / Gamma_GeV

def width_from_lifetime(tau_s):
    hbar = 6.582119569e-25
    return hbar / tau_s

def lab_decay_length(tau_s, gamma, beta):
    c = 2.998e8
    return gamma * beta * c * tau_s

print(f"{lifetime_from_width(2.4952):.4e}")
print(f"{width_from_lifetime(2.197e-6):.4e}")
print(round(lab_decay_length(2.6e-8, 10, 1.0), 2))
print(f"{lifetime_from_width(4.07e-3):.4e}")
`,

	tests: [
		{
			name: "lifetime_from_width(2.4952) — Z boson lifetime ≈ 2.6379e-25 s",
			code: `{{FUNC}}
print(f"{lifetime_from_width(2.4952):.4e}")`,
			expected: "2.6379e-25\n",
		},
		{
			name: "width_from_lifetime(2.197e-6) — muon decay width ≈ 2.9960e-19 GeV",
			code: `{{FUNC}}
print(f"{width_from_lifetime(2.197e-6):.4e}")`,
			expected: "2.9960e-19\n",
		},
		{
			name: "lab_decay_length(2.6e-8, 10, 1.0) — pion with γ=10 travels ≈ 77.95 m",
			code: `{{FUNC}}
print(round(lab_decay_length(2.6e-8, 10, 1.0), 2))`,
			expected: "77.95\n",
		},
		{
			name: "lifetime_from_width(4.07e-3) — Higgs boson lifetime ≈ 1.6172e-22 s",
			code: `{{FUNC}}
print(f"{lifetime_from_width(4.07e-3):.4e}")`,
			expected: "1.6172e-22\n",
		},
	],
};
