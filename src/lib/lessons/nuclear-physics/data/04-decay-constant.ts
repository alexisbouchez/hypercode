import type { Lesson } from "../../types";

export const decayConstant: Lesson = {
	id: "decay-constant",
	title: "Decay Constant and Half-Life",
	chapterId: "radioactive-decay",
	content: `## Decay Constant and Half-Life

Radioactive decay is a **stochastic** process — any individual nucleus may decay at any moment, but large ensembles follow a precise statistical law.

### The Decay Law

The rate of decay is proportional to the number of nuclei present:

$$\\frac{dN}{dt} = -\\lambda N$$

where $\\lambda$ is the **decay constant** (units: s⁻¹). This gives the exponential solution:

$$N(t) = N_0 \\, e^{-\\lambda t}$$

### Half-Life

The **half-life** $t_{1/2}$ is the time for half the nuclei to decay:

$$t_{1/2} = \\frac{\\ln 2}{\\lambda}$$

Conversely, given the half-life:

$$\\lambda = \\frac{\\ln 2}{t_{1/2}}$$

### Mean Lifetime

The **mean lifetime** $\\tau$ is the average time a nucleus survives before decaying:

$$\\tau = \\frac{1}{\\lambda} = \\frac{t_{1/2}}{\\ln 2} \\approx 1.4427 \\cdot t_{1/2}$$

### Activity

The **activity** $A$ is the number of decays per second (unit: Becquerel, Bq):

$$A = \\lambda N$$

| Isotope | Half-Life | $\\lambda$ (s⁻¹) |
|---------|-----------|----------------|
| C-14   | 5730 yr   | 3.83 × 10⁻¹²   |
| Ra-226 | 1600 yr   | 1.37 × 10⁻¹¹   |
| Po-210 | 138.4 d   | 5.80 × 10⁻⁸    |
| Bi-212 | 60.55 min | 1.91 × 10⁻⁴    |

### Your Task

Implement three functions using \`math.log(2)\` for $\\ln 2$. All constants must be defined **inside** each function.

- \`decay_constant(half_life_s)\` — returns $\\lambda = \\ln 2 / t_{1/2}$ in s⁻¹
- \`half_life(lambda_s)\` — returns $t_{1/2} = \\ln 2 / \\lambda$ in s
- \`mean_lifetime(half_life_s)\` — returns $\\tau = t_{1/2} / \\ln 2$ in s`,

	starterCode: `import math

def decay_constant(half_life_s):
    # TODO: return ln(2) / half_life_s
    pass

def half_life(lambda_s):
    # TODO: return ln(2) / lambda_s
    pass

def mean_lifetime(half_life_s):
    # TODO: return half_life_s / ln(2)
    pass

# C-14: t_half = 5730 * 365.25 * 24 * 3600 seconds
t_c14 = 5730 * 365.25 * 24 * 3600
print(round(decay_constant(t_c14) * 1e12, 4))

# Ra-226: t_half = 1600 * 365.25 * 24 * 3600 seconds
t_ra226 = 1600 * 365.25 * 24 * 3600
print(round(decay_constant(t_ra226) * 1e11, 4))

# Po-210: t_half = 138.4 * 24 * 3600 seconds
t_po210 = 138.4 * 24 * 3600
print(round(decay_constant(t_po210) * 1e8, 4))

# Bi-212: t_half = 60.55 * 60 seconds
t_bi212 = 60.55 * 60
print(round(decay_constant(t_bi212) * 1e4, 4))
`,

	solution: `import math

def decay_constant(half_life_s):
    return math.log(2) / half_life_s

def half_life(lambda_s):
    return math.log(2) / lambda_s

def mean_lifetime(half_life_s):
    return half_life_s / math.log(2)

# C-14: t_half = 5730 * 365.25 * 24 * 3600 seconds
t_c14 = 5730 * 365.25 * 24 * 3600
print(round(decay_constant(t_c14) * 1e12, 4))

# Ra-226: t_half = 1600 * 365.25 * 24 * 3600 seconds
t_ra226 = 1600 * 365.25 * 24 * 3600
print(round(decay_constant(t_ra226) * 1e11, 4))

# Po-210: t_half = 138.4 * 24 * 3600 seconds
t_po210 = 138.4 * 24 * 3600
print(round(decay_constant(t_po210) * 1e8, 4))

# Bi-212: t_half = 60.55 * 60 seconds
t_bi212 = 60.55 * 60
print(round(decay_constant(t_bi212) * 1e4, 4))
`,

	tests: [
		{
			name: "decay_constant for C-14 (t½ = 5730 yr) ≈ 3.8332 × 10⁻¹² s⁻¹",
			code: `{{FUNC}}
t_c14 = 5730 * 365.25 * 24 * 3600
print(round(decay_constant(t_c14) * 1e12, 4))`,
			expected: "3.8332\n",
		},
		{
			name: "decay_constant for Ra-226 (t½ = 1600 yr) ≈ 1.3728 × 10⁻¹¹ s⁻¹",
			code: `{{FUNC}}
t_ra226 = 1600 * 365.25 * 24 * 3600
print(round(decay_constant(t_ra226) * 1e11, 4))`,
			expected: "1.3728\n",
		},
		{
			name: "decay_constant for Po-210 (t½ = 138.4 days) ≈ 5.7966 × 10⁻⁸ s⁻¹",
			code: `{{FUNC}}
t_po210 = 138.4 * 24 * 3600
print(round(decay_constant(t_po210) * 1e8, 4))`,
			expected: "5.7966\n",
		},
		{
			name: "decay_constant for Bi-212 (t½ = 60.55 min) ≈ 1.9079 × 10⁻⁴ s⁻¹",
			code: `{{FUNC}}
t_bi212 = 60.55 * 60
print(round(decay_constant(t_bi212) * 1e4, 4))`,
			expected: "1.9079\n",
		},
	],
};
