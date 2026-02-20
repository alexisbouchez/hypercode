import type { Lesson } from "../../types";

export const chiSquare: Lesson = {
	id: "chi-square",
	title: "Chi-Square Test",
	chapterId: "inference",
	content: `## Testing Categorical Data

The **chi-square goodness-of-fit test** checks whether observed frequencies match expected frequencies for a categorical variable.

\`\`\`python
from scipy import stats

# Roll a die 100 times. Is it fair?
observed = [20, 15, 18, 22, 17, 8]  # 6 categories
chi2, p = stats.chisquare(observed)  # default: equal expected frequencies
print(round(chi2, 4))   # chi-square statistic
print(p < 0.05)         # is the die unfair?
\`\`\`

### How It Works

For each category:
\`\`\`
chi2_contribution = (observed - expected)² / expected
\`\`\`

Sum all contributions to get the chi-square statistic. If observed matches expected perfectly, chi2 = 0.

### Degrees of Freedom

For k categories: df = k − 1. More categories → larger chi2 needed for significance.

### Custom Expected Frequencies

\`\`\`python
# Non-uniform expected: 50%, 30%, 20%
observed = [48, 32, 20]
expected = [50, 30, 20]
chi2, p = stats.chisquare(observed, f_exp=expected)
\`\`\`

### Your Task

Implement \`chi_square_test(observed)\` that tests whether the observed frequencies are **uniformly distributed** (equal expected frequency for each category). Print the chi-square statistic (rounded to 4 decimal places) and the p-value (rounded to 4 decimal places).`,

	starterCode: `from scipy import stats

def chi_square_test(observed):
    # Use stats.chisquare with uniform expected frequencies
    # Print chi2 (round 4) and p_value (round 4)
    pass

chi_square_test([25, 25, 25, 25])
`,

	solution: `from scipy import stats

def chi_square_test(observed):
    chi2, p = stats.chisquare(observed)
    print(round(float(chi2), 4))
    print(round(float(p), 4))

chi_square_test([25, 25, 25, 25])
`,

	tests: [
		{
			name: "uniform frequencies → chi2=0.0, p=1.0",
			expected: "0.0\n1.0\n",
		},
		{
			name: "chi_square_test([20,30,25,25]) → chi2=2.0, p=0.5724",
			code: `{{FUNC}}
chi_square_test([20, 30, 25, 25])`,
			expected: "2.0\n0.5724\n",
		},
		{
			name: "highly skewed → p < 0.05",
			code: `{{FUNC}}
from scipy import stats
chi2, p = stats.chisquare([95, 1, 1, 3])
print(p < 0.05)`,
			expected: "True\n",
		},
		{
			name: "chi2 is always non-negative",
			code: `{{FUNC}}
from scipy import stats
chi2, _ = stats.chisquare([10, 20, 30])
print(chi2 >= 0)`,
			expected: "True\n",
		},
	],
};
