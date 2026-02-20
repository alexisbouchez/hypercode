import type { Lesson } from "../../types";

export const ttestTwoSample: Lesson = {
	id: "ttest-two-sample",
	title: "Two-Sample t-Test",
	chapterId: "inference",
	content: `## Comparing Two Groups

The **two-sample t-test** checks whether two independent groups have different means.

**Hypotheses:**
- H₀: μ₁ = μ₂ (no difference between groups)
- H₁: μ₁ ≠ μ₂

\`\`\`python
from scipy import stats

group_a = [1, 2, 3, 4, 5]
group_b = [6, 7, 8, 9, 10]

t_stat, p_value = stats.ttest_ind(group_a, group_b, equal_var=False)
print(round(t_stat, 4))   # -5.0
print(p_value < 0.05)     # True — groups differ significantly
\`\`\`

### Welch's t-Test (\`equal_var=False\`)

Use \`equal_var=False\` (Welch's t-test) unless you have strong reason to assume equal variances. Welch's test is more robust and is the recommended default.

### Effect Size vs Statistical Significance

A statistically significant result (p < 0.05) does not necessarily mean the difference is **practically important**. With large samples, even tiny differences become significant.

**Cohen's d** measures effect size:
\`\`\`
d = (mean_a - mean_b) / pooled_std
\`\`\`
- d ≈ 0.2: small effect
- d ≈ 0.5: medium effect
- d ≈ 0.8: large effect

### Same Groups → t = 0

When both groups are identical, t = 0 and p = 1.0:

\`\`\`python
t, p = stats.ttest_ind([1,2,3], [1,2,3], equal_var=False)
print(t)   # 0.0
print(p)   # 1.0
\`\`\`

### Your Task

Implement \`ttest_independent(group_a, group_b)\` that prints the t-statistic (rounded to 4 decimal places) and whether the result is significant (\`p < 0.05\`).`,

	starterCode: `from scipy import stats

def ttest_independent(group_a, group_b):
    # Print t_stat (round 4) and whether p < 0.05 (True/False)
    pass

ttest_independent([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])
`,

	solution: `from scipy import stats

def ttest_independent(group_a, group_b):
    t_stat, p_value = stats.ttest_ind(group_a, group_b, equal_var=False)
    print(round(float(t_stat), 4))
    print(p_value < 0.05)

ttest_independent([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])
`,

	tests: [
		{
			name: "ttest_independent([1..5], [6..10]) → t=-5.0, significant",
			expected: "-5.0\nTrue\n",
		},
		{
			name: "identical groups → t=0.0, not significant",
			code: `{{FUNC}}
ttest_independent([1, 2, 3, 4, 5], [1, 2, 3, 4, 5])`,
			expected: "0.0\nFalse\n",
		},
		{
			name: "reversing groups negates the t-statistic",
			code: `{{FUNC}}
from scipy import stats
t1, _ = stats.ttest_ind([1,2,3,4,5], [6,7,8,9,10], equal_var=False)
t2, _ = stats.ttest_ind([6,7,8,9,10], [1,2,3,4,5], equal_var=False)
print(round(float(t1), 4))
print(round(float(t2), 4))`,
			expected: "-5.0\n5.0\n",
		},
		{
			name: "well-separated groups → always significant",
			code: `{{FUNC}}
from scipy import stats
_, p = stats.ttest_ind([1, 2, 3], [100, 101, 102], equal_var=False)
print(p < 0.001)`,
			expected: "True\n",
		},
	],
};
