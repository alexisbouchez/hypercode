import type { Lesson } from "../../types";

export const normalDistribution: Lesson = {
	id: "normal-distribution",
	title: "Normal Distribution",
	chapterId: "distributions",
	content: `## The Bell Curve

The **normal distribution** is the most important distribution in statistics. It is symmetric around the mean and fully described by two parameters: mean (μ) and standard deviation (σ).

\`\`\`python
from scipy import stats

# Standard normal: μ=0, σ=1
dist = stats.norm(loc=0, scale=1)

# Probability that X ≤ 0 (CDF)
print(dist.cdf(0))    # 0.5

# Probability density at x=0 (PDF)
print(round(dist.pdf(0), 4))  # 0.3989
\`\`\`

### CDF vs PDF

- **PDF** (probability density function): the height of the curve at x. Not a probability itself.
- **CDF** (cumulative distribution function): P(X ≤ x). This IS a probability (0 to 1).

### The 68-95-99.7 Rule

For a normal distribution N(μ, σ):
- P(μ - σ ≤ X ≤ μ + σ) ≈ **68%**
- P(μ - 2σ ≤ X ≤ μ + 2σ) ≈ **95%**
- P(μ - 3σ ≤ X ≤ μ + 3σ) ≈ **99.7%**

### Your Task

Implement \`normal_stats(mu, sigma, x)\` that prints:
1. \`CDF(x)\` — the probability that X ≤ x (rounded to 4 decimal places)
2. \`PDF(x)\` — the density at x (rounded to 4 decimal places)`,

	starterCode: `from scipy import stats

def normal_stats(mu, sigma, x):
    # Print CDF(x) and PDF(x) of N(mu, sigma), each rounded to 4 decimal places
    pass

normal_stats(0, 1, 0)
`,

	solution: `from scipy import stats

def normal_stats(mu, sigma, x):
    dist = stats.norm(loc=mu, scale=sigma)
    print(round(float(dist.cdf(x)), 4))
    print(round(float(dist.pdf(x)), 4))

normal_stats(0, 1, 0)
`,

	tests: [
		{
			name: "normal_stats(0,1,0) → CDF=0.5, PDF=0.3989",
			expected: "0.5\n0.3989\n",
		},
		{
			name: "normal_stats(0,1,1) → CDF=0.8413, PDF=0.242",
			code: `{{FUNC}}
normal_stats(0, 1, 1)`,
			expected: "0.8413\n0.242\n",
		},
		{
			name: "CDF at mean is always 0.5",
			code: `{{FUNC}}
normal_stats(10, 2, 10)`,
			expected: "0.5\n0.1995\n",
		},
		{
			name: "normal_stats(0,1,-1) → CDF=0.1587, PDF=0.242",
			code: `{{FUNC}}
normal_stats(0, 1, -1)`,
			expected: "0.1587\n0.242\n",
		},
	],
};
