import type { Lesson } from "../../types";

export const radioactiveDecay: Lesson = {
	id: "radioactive-decay",
	title: "Radioactive Decay Law",
	chapterId: "radioactive-decay",
	content: `## Radioactive Decay Law

In the previous lesson we derived the decay constant $\\lambda$. Now we use it to track how a population of nuclei evolves over time.

### Number of Nuclei Over Time

Solving $dN/dt = -\\lambda N$ gives:

$$N(t) = N_0 \\, e^{-\\lambda t}$$

where $N_0$ is the initial number of nuclei. Equivalently, using the half-life $t_{1/2}$:

$$N(t) = N_0 \\left(\\frac{1}{2}\\right)^{t/t_{1/2}}$$

### Fraction Remaining

The fraction of original nuclei still present:

$$\\frac{N(t)}{N_0} = e^{-\\lambda t}$$

After exactly one half-life: $e^{-\\lambda \\cdot t_{1/2}} = e^{-\\ln 2} = 0.5$

After $n$ half-lives: $\\left(\\frac{1}{2}\\right)^n$

### Activity Over Time

The **activity** (decay rate) also follows exponential decay:

$$A(t) = \\lambda N(t) = \\lambda N_0 \\, e^{-\\lambda t} = A_0 \\, e^{-\\lambda t}$$

where $A_0 = \\lambda N_0$ is the initial activity in Becquerel (Bq).

### Example: Carbon-14 Dating

C-14 has a half-life of 5730 years. A living organism maintains a constant C-14/C-12 ratio. After death, C-14 decays with no replenishment. Measuring the fraction remaining tells us the age:

$$t = -\\frac{1}{\\lambda} \\ln\\left(\\frac{N}{N_0}\\right)$$

After one half-life (5730 yr), exactly half the C-14 remains. After two half-lives (11460 yr), one quarter remains.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`nuclei_remaining(N0, lambda_s, t)\` — returns $N_0 \\, e^{-\\lambda t}$
- \`fraction_remaining(lambda_s, t)\` — returns $e^{-\\lambda t}$
- \`activity(N0, lambda_s, t)\` — returns $\\lambda N_0 \\, e^{-\\lambda t}$ in Bq`,

	starterCode: `import math

def nuclei_remaining(N0, lambda_s, t):
    # TODO: return N0 * exp(-lambda_s * t)
    pass

def fraction_remaining(lambda_s, t):
    # TODO: return exp(-lambda_s * t)
    pass

def activity(N0, lambda_s, t):
    # TODO: return lambda_s * N0 * exp(-lambda_s * t)
    pass

# After 1 half-life (lambda = ln(2)/100, t = 100)
print(fraction_remaining(math.log(2) / 100, 100))

# After 2 half-lives
print(fraction_remaining(math.log(2) / 100, 200))

# C-14 nuclei remaining after 5730 years from N0=1e24
t_c14_s = 5730 * 365.25 * 24 * 3600
lam_c14 = math.log(2) / t_c14_s
print(round(nuclei_remaining(1e24, lam_c14, t_c14_s) / 1e23, 4))

# Activity at t=0 with N0=1,000,000 atoms and half-life=100s
print(round(activity(1000000, math.log(2) / 100, 0), 4))
`,

	solution: `import math

def nuclei_remaining(N0, lambda_s, t):
    return N0 * math.exp(-lambda_s * t)

def fraction_remaining(lambda_s, t):
    return math.exp(-lambda_s * t)

def activity(N0, lambda_s, t):
    return lambda_s * N0 * math.exp(-lambda_s * t)

# After 1 half-life (lambda = ln(2)/100, t = 100)
print(fraction_remaining(math.log(2) / 100, 100))

# After 2 half-lives
print(fraction_remaining(math.log(2) / 100, 200))

# C-14 nuclei remaining after 5730 years from N0=1e24
t_c14_s = 5730 * 365.25 * 24 * 3600
lam_c14 = math.log(2) / t_c14_s
print(round(nuclei_remaining(1e24, lam_c14, t_c14_s) / 1e23, 4))

# Activity at t=0 with N0=1,000,000 atoms and half-life=100s
print(round(activity(1000000, math.log(2) / 100, 0), 4))
`,

	tests: [
		{
			name: "fraction_remaining after exactly 1 half-life = 0.5",
			code: `{{FUNC}}
print(fraction_remaining(math.log(2) / 100, 100))`,
			expected: "0.5\n",
		},
		{
			name: "fraction_remaining after 2 half-lives = 0.25",
			code: `{{FUNC}}
print(fraction_remaining(math.log(2) / 100, 200))`,
			expected: "0.25\n",
		},
		{
			name: "nuclei_remaining: C-14 after 5730 yr from 1e24 atoms → 5.0 × 10²³",
			code: `{{FUNC}}
t_c14_s = 5730 * 365.25 * 24 * 3600
lam_c14 = math.log(2) / t_c14_s
print(round(nuclei_remaining(1e24, lam_c14, t_c14_s) / 1e23, 4))`,
			expected: "5.0\n",
		},
		{
			name: "activity at t=0 with N0=1e6 atoms and t½=100s ≈ 6931.4718 Bq",
			code: `{{FUNC}}
print(round(activity(1000000, math.log(2) / 100, 0), 4))`,
			expected: "6931.4718\n",
		},
	],
};
