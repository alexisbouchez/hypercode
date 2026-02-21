import type { Lesson } from "../../types";

export const chiSquare: Lesson = {
	id: "chi-square",
	title: "Chi-Square Test",
	chapterId: "inference",
	content: `## Testing Categorical Data

The **chi-square goodness-of-fit test** checks whether observed frequencies match expected frequencies for a categorical variable.

\`\`\`python
import math

# Roll a die 100 times. Is it fair?
observed = [20, 15, 18, 22, 17, 8]  # 6 categories
n = len(observed)
total = sum(observed)
expected = total / n  # uniform: each category equally likely

chi2 = sum((o - expected)**2 / expected for o in observed)
print(round(chi2, 4))   # chi-square statistic
\`\`\`

### How It Works

For each category, compute:

$$\chi^2 = \sum_{i=1}^k \frac{(O_i - E_i)^2}{E_i}$$

where $O_i$ is the observed count and $E_i$ is the expected count. If observed matches expected perfectly, $\chi^2 = 0$.

### Degrees of Freedom

For $k$ categories: $df = k - 1$. More categories require a larger $\chi^2$ value for significance.

### Custom Expected Frequencies

You can also test against non-uniform expectations by providing your own expected counts.

### Your Task

Implement \`chi_square_test(observed)\` that tests whether the observed frequencies are **uniformly distributed** (equal expected frequency for each category). Print the $\chi^2$ statistic (rounded to 4 decimal places) and the $p$-value (rounded to 4 decimal places).`,

	starterCode: `import math

def chi_square_test(observed):
    # Compute chi2 with uniform expected frequencies
    # Print chi2 (round 4) and p_value (round 4)
    pass

chi_square_test([25, 25, 25, 25])
`,

	solution: `import math

def _chi2_cdf(x, df):
    if x <= 0: return 0.0
    a, t = df/2.0, x/2.0
    if t > a + 100: return 1.0  # x far into the tail: CDF ≈ 1
    term = math.exp(-t + a*math.log(t) - math.lgamma(a+1))
    s = term
    for n in range(1, 300):
        term *= t/(a+n); s += term
        if term < 1e-12: break
    return min(s, 1.0)

def chi_square_test(observed):
    n = len(observed)
    total = sum(observed)
    expected = total / n
    chi2 = sum((o - expected)**2 / expected for o in observed)
    df = n - 1
    p = round(1 - _chi2_cdf(chi2, df), 4)
    print(round(chi2, 4))
    print(p)

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
obs = [95, 1, 1, 3]
n = len(obs)
total = sum(obs)
expected = total / n
chi2 = sum((o - expected)**2 / expected for o in obs)
p = 1 - _chi2_cdf(chi2, n - 1)
print(p < 0.05)`,
			expected: "True\n",
		},
		{
			name: "chi2 is always non-negative",
			code: `{{FUNC}}
obs = [10, 20, 30]
n = len(obs)
total = sum(obs)
expected = total / n
chi2 = sum((o - expected)**2 / expected for o in obs)
print(chi2 >= 0)`,
			expected: "True\n",
		},
	],
};
