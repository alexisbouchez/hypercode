import type { Lesson } from "../../types";

export const correlation: Lesson = {
	id: "correlation",
	title: "Correlation",
	chapterId: "regression",
	content: `## Measuring Linear Relationships

**Pearson's r** measures the strength and direction of a linear relationship between two variables. It ranges from −1 to +1.

\`\`\`python
from scipy import stats

x = [1, 2, 3, 4, 5]
y = [2, 4, 6, 8, 10]   # perfect positive relationship

r, p = stats.pearsonr(x, y)
print(round(r, 4))   # 1.0
print(p < 0.001)     # True
\`\`\`

### Interpreting r

| r value | Interpretation |
|---------|---------------|
| r = 1.0 | Perfect positive linear relationship |
| r = 0.7 | Strong positive |
| r = 0.3 | Weak positive |
| r = 0.0 | No linear relationship |
| r = −0.7 | Strong negative |
| r = −1.0 | Perfect negative linear relationship |

### Correlation ≠ Causation

A high correlation between X and Y does not mean X causes Y. There may be a confounding variable, or the relationship may be coincidental.

### Correlation Requires Linearity

Pearson's r only measures **linear** relationships. Two variables can have a strong non-linear relationship (e.g., quadratic) while r ≈ 0.

\`\`\`python
import numpy as np
x = np.linspace(-3, 3, 100)
y = x ** 2   # perfect quadratic relationship
r, _ = stats.pearsonr(x, y)
print(round(r, 4))   # ≈ 0.0 — no linear correlation!
\`\`\`

### Your Task

Implement \`pearson_r(x, y)\` that prints the correlation coefficient \`r\` (rounded to 4 decimal places) and whether the relationship is statistically significant (\`p < 0.001\`).`,

	starterCode: `from scipy import stats

def pearson_r(x, y):
    # Print r (round 4) and whether p < 0.001 (True/False)
    pass

pearson_r([1, 2, 3, 4, 5], [2, 4, 6, 8, 10])
`,

	solution: `from scipy import stats

def pearson_r(x, y):
    r, p = stats.pearsonr(x, y)
    print(round(float(r), 4))
    print(p < 0.001)

pearson_r([1, 2, 3, 4, 5], [2, 4, 6, 8, 10])
`,

	tests: [
		{
			name: "perfect positive correlation → r=1.0, significant",
			expected: "1.0\nTrue\n",
		},
		{
			name: "perfect negative correlation → r=-1.0, significant",
			code: `{{FUNC}}
pearson_r([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])`,
			expected: "-1.0\nTrue\n",
		},
		{
			name: "no correlation → |r| small, not significant",
			code: `{{FUNC}}
from scipy import stats
r, p = stats.pearsonr([1, 2, 3, 4, 5], [3, 1, 4, 1, 5])
print(abs(r) < 0.5)
print(p > 0.05)`,
			expected: "True\nTrue\n",
		},
		{
			name: "r is between -1 and 1",
			code: `{{FUNC}}
from scipy import stats
r, _ = stats.pearsonr([1, 3, 2, 5, 4], [2, 5, 3, 8, 6])
print(-1.0 <= r <= 1.0)`,
			expected: "True\n",
		},
	],
};
