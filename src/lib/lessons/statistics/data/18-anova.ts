import type { Lesson } from "../../types";

export const anova: Lesson = {
	id: "anova",
	title: "One-Way ANOVA",
	chapterId: "inference",
	content: `## One-Way ANOVA

**Analysis of Variance (ANOVA)** tests whether the means of three or more groups are equal. It works by comparing the **variance between groups** to the **variance within groups**.

**Hypotheses:**
- $H_0$: all group means are equal ($\\mu_1 = \\mu_2 = \\cdots = \\mu_k$)
- $H_1$: at least one group mean differs

### The F-Statistic

$$F = \\frac{MS_{\\text{between}}}{MS_{\\text{within}}} = \\frac{SS_{\\text{between}} / (k-1)}{SS_{\\text{within}} / (N-k)}$$

where:
- $k$ = number of groups, $N$ = total observations
- $SS_{\\text{between}} = \\sum_i n_i (\\bar{x}_i - \\bar{x})^2$ — variation due to group differences
- $SS_{\\text{within}} = \\sum_i \\sum_j (x_{ij} - \\bar{x}_i)^2$ — variation within groups

A large $F$ means the groups differ more than random variation would explain.

### Example

Groups: $[1,2,3]$, $[4,5,6]$, $[7,8,9]$

| Source | SS | df | MS | F |
|--------|----|----|----|----|
| Between | 54 | 2 | 27 | **27.0** |
| Within | 6 | 6 | 1 | |

$$F = 27.0$$

### Implementation

\`\`\`python
def anova_f(groups):
    n_total = sum(len(g) for g in groups)
    k = len(groups)
    grand_mean = sum(sum(g) for g in groups) / n_total
    group_means = [sum(g)/len(g) for g in groups]
    ss_between = sum(len(g)*(m - grand_mean)**2
                     for g, m in zip(groups, group_means))
    ss_within = sum((x - m)**2
                    for g, m in zip(groups, group_means)
                    for x in g)
    ms_between = ss_between / (k - 1)
    ms_within = ss_within / (n_total - k)
    return round(ms_between / ms_within, 4)
\`\`\`

### Your Task

Implement \`anova_f(groups)\` that computes the F-statistic from a list of groups (each a list of numbers).`,

	starterCode: `def anova_f(groups):
    # F = MS_between / MS_within
    return 0.0

print(anova_f([[1,2,3],[4,5,6],[7,8,9]]))   # 27.0
print(anova_f([[1,2,3],[1,2,3]]))            # 0.0 (same means)
`,

	solution: `def anova_f(groups):
    n_total = sum(len(g) for g in groups)
    k = len(groups)
    grand_mean = sum(sum(g) for g in groups) / n_total
    group_means = [sum(g) / len(g) for g in groups]
    ss_between = sum(len(g) * (m - grand_mean)**2
                     for g, m in zip(groups, group_means))
    ss_within = sum((x - m)**2
                    for g, m in zip(groups, group_means)
                    for x in g)
    ms_between = ss_between / (k - 1)
    ms_within = ss_within / (n_total - k)
    return round(ms_between / ms_within, 4)

print(anova_f([[1,2,3],[4,5,6],[7,8,9]]))
print(anova_f([[1,2,3],[1,2,3]]))
`,

	tests: [
		{
			name: "clearly separated groups → F=27.0; identical groups → F=0.0",
			expected: "27.0\n0.0\n",
		},
		{
			name: "F=27.0 for [[1,2,3],[4,5,6],[7,8,9]]",
			code: `{{FUNC}}
print(anova_f([[1, 2, 3], [4, 5, 6], [7, 8, 9]]))`,
			expected: "27.0\n",
		},
		{
			name: "two groups [1,2,3] vs [7,8,9] → large F",
			code: `{{FUNC}}
f = anova_f([[1, 2, 3], [7, 8, 9]])
print(f > 10)`,
			expected: "True\n",
		},
		{
			name: "balanced 3-group design [[1,2],[3,4],[5,6]] → F=16.0",
			code: `{{FUNC}}
print(anova_f([[1, 2], [3, 4], [5, 6]]))`,
			expected: "16.0\n",
		},
	],
};
