import type { Lesson } from "../../types";

export const branchingRatio: Lesson = {
	id: "branching-ratio",
	title: "Branching Ratios and Partial Widths",
	chapterId: "decays",
	content: `## Branching Ratios and Partial Widths

An unstable particle can decay via multiple channels. Each channel $i$ has a **partial width** $\\Gamma_i$ — the rate of decay into that mode. The **total decay width** is:

$$\\Gamma_{\\text{total}} = \\sum_i \\Gamma_i$$

The mean lifetime is $\\tau = \\hbar / \\Gamma_{\\text{total}}$. Each decay mode's probability is its **branching ratio**:

$$\\text{BR}_i = \\frac{\\Gamma_i}{\\Gamma_{\\text{total}}}$$

### The Z Boson

The $Z$ boson ($M_Z \\approx 91.2$ GeV, $\\Gamma_Z \\approx 2.4952$ GeV) decays to:

| Mode | Partial Width | Branching Ratio |
|------|--------------|----------------|
| $Z \\to e^+e^-$ | 83.91 MeV | 3.363% |
| $Z \\to \\mu^+\\mu^-$ | 83.99 MeV | 3.366% |
| $Z \\to \\tau^+\\tau^-$ | 84.08 MeV | 3.370% |
| $Z \\to$ hadrons | 1740.8 MeV | 69.82% |
| $Z \\to \\nu\\bar{\\nu}$ (×3) | 501.55 MeV | 20.00% |

### Counting Neutrino Generations

The invisible width $\\Gamma_{\\text{inv}} = \\Gamma_Z - \\Gamma_{\\text{visible}}$ is carried by neutrinos. Since we know $\\Gamma(Z \\to \\nu_e \\bar{\\nu}_e) \\approx 167.17$ MeV from the Standard Model, the number of light neutrino generations is:

$$N_\\nu = \\frac{\\Gamma_{\\text{inv}}}{\\Gamma(Z \\to \\nu\\bar{\\nu})} \\approx 3$$

This LEP measurement proved there are exactly three neutrino families.

### Your Task

Implement three functions. All constants must be defined **inside** each function body.
- \`branching_ratio(partial_width_GeV, total_width_GeV)\`
- \`partial_width(total_width_GeV, branching_ratio)\`
- \`number_of_neutrino_generations(Gamma_inv_GeV, Gamma_Z_to_nunu_GeV)\` — returns an integer`,

	starterCode: `def branching_ratio(partial_width_GeV, total_width_GeV):
    # TODO: return partial / total
    pass

def partial_width(total_width_GeV, branching_ratio):
    # TODO: return total * BR
    pass

def number_of_neutrino_generations(Gamma_inv_GeV, Gamma_Z_to_nunu_GeV):
    # TODO: return round(invisible / per-neutrino)
    pass
`,

	solution: `def branching_ratio(partial_width_GeV, total_width_GeV):
    return partial_width_GeV / total_width_GeV

def partial_width(total_width_GeV, branching_ratio):
    return total_width_GeV * branching_ratio

def number_of_neutrino_generations(Gamma_inv_GeV, Gamma_Z_to_nunu_GeV):
    return round(Gamma_inv_GeV / Gamma_Z_to_nunu_GeV)
`,

	tests: [
		{
			name: "branching_ratio(Z→ee): 0.08391 / 2.4952 ≈ 0.033629",
			code: `{{FUNC}}
print(round(branching_ratio(0.08391, 2.4952), 6))`,
			expected: "0.033629\n",
		},
		{
			name: "partial_width(Z, 0.6982) hadronic ≈ 1.7421 GeV",
			code: `{{FUNC}}
print(round(partial_width(2.4952, 0.6982), 4))`,
			expected: "1.7421\n",
		},
		{
			name: "number_of_neutrino_generations = 3 (from LEP data)",
			code: `{{FUNC}}
print(number_of_neutrino_generations(0.50155, 0.16717))`,
			expected: "3\n",
		},
		{
			name: "branching_ratio(W→eν): 0.2267 / 2.085 ≈ 0.108729",
			code: `{{FUNC}}
print(round(branching_ratio(0.2267, 2.085), 6))`,
			expected: "0.108729\n",
		},
	],
};
