import type { Lesson } from "../../types";

export const ttestOneSample: Lesson = {
	id: "ttest-one-sample",
	title: "One-Sample t-Test",
	chapterId: "inference",
	content: `## Hypothesis Testing

The **one-sample t-test** checks whether the mean of your data is significantly different from a hypothesized value.

**Hypotheses:**
- H₀ (null): μ = μ₀ (the mean equals the hypothesized value)
- H₁ (alternative): μ ≠ μ₀

\`\`\`python
from scipy import stats

data = [2.1, 2.5, 2.3, 2.8, 2.4]
mu_0 = 2.0   # hypothesized mean

t_stat, p_value = stats.ttest_1samp(data, mu_0)
print(round(t_stat, 4))   # 4.0
print(p_value < 0.05)     # True — reject H₀
\`\`\`

### The t-Statistic

\`\`\`
t = (x̄ - μ₀) / (s / √n)
\`\`\`

A large |t| means the sample mean is far from μ₀ relative to the data's variability.

### The p-value

The p-value is the probability of observing a t-statistic this extreme (or more) if H₀ were true.

- **p < 0.05** → statistically significant (reject H₀ at 5% significance level)
- **p ≥ 0.05** → insufficient evidence to reject H₀

### Special Case: Testing Against the Sample Mean

When μ₀ equals the sample mean exactly, t = 0 and p = 1.0:

\`\`\`python
t, p = stats.ttest_1samp([1, 2, 3, 4, 5], popmean=3)
print(t)   # 0.0
print(p)   # 1.0
\`\`\`

### Your Task

Implement \`ttest_one_sample(data, mu)\` that prints the t-statistic (rounded to 4 decimal places) and whether the result is significant (\`p < 0.05\`).`,

	starterCode: `from scipy import stats

def ttest_one_sample(data, mu):
    # Print t_stat (round 4) and whether p < 0.05 (True/False)
    pass

ttest_one_sample([1, 2, 3, 4, 5], 3)
`,

	solution: `from scipy import stats

def ttest_one_sample(data, mu):
    t_stat, p_value = stats.ttest_1samp(data, mu)
    print(round(float(t_stat), 4))
    print(p_value < 0.05)

ttest_one_sample([1, 2, 3, 4, 5], 3)
`,

	tests: [
		{
			name: "ttest_one_sample([1,2,3,4,5], 3) → t=0.0, not significant",
			expected: "0.0\nFalse\n",
		},
		{
			name: "ttest_one_sample([1,2,3,4,5], 5) → t=-2.8284, significant",
			code: `{{FUNC}}
ttest_one_sample([1, 2, 3, 4, 5], 5)`,
			expected: "-2.8284\nTrue\n",
		},
		{
			name: "ttest_one_sample([2,3,4], 3) → t=0.0, not significant",
			code: `{{FUNC}}
ttest_one_sample([2, 3, 4], 3)`,
			expected: "0.0\nFalse\n",
		},
		{
			name: "ttest_one_sample([2,2,3,4,4], 0) → t=6.7082, significant",
			code: `{{FUNC}}
ttest_one_sample([2, 2, 3, 4, 4], 0)`,
			expected: "6.7082\nTrue\n",
		},
	],
};
