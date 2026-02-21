import type { Lesson } from "../../types";

export const ttestOneSample: Lesson = {
	id: "ttest-one-sample",
	title: "One-Sample t-Test",
	chapterId: "inference",
	content: `## Hypothesis Testing

The **one-sample t-test** checks whether the mean of your data is significantly different from a hypothesized value.

**Hypotheses:**
- $H_0$ (null): $\mu = \mu_0$ (the mean equals the hypothesized value)
- $H_1$ (alternative): $\mu \neq \mu_0$

\`\`\`python
import math, statistics

data = [2.1, 2.5, 2.3, 2.8, 2.4]
mu_0 = 2.0   # hypothesized mean
n = len(data)

t_stat = (statistics.fmean(data) - mu_0) / (statistics.stdev(data) / math.sqrt(n))
print(round(t_stat, 4))   # 4.0
\`\`\`

### The t-Statistic

$$t = \frac{\bar{x} - \mu_0}{s / \sqrt{n}}$$

A large $|t|$ means the sample mean is far from $\mu_0$ relative to the data's variability.

### The p-value

The $p$-value is the probability of observing a $t$-statistic this extreme (or more) if $H_0$ were true.

- $p < 0.05$ → statistically significant (reject $H_0$ at 5% significance level)
- $p \geq 0.05$ → insufficient evidence to reject $H_0$

### Special Case: Testing Against the Sample Mean

When $\mu_0$ equals the sample mean exactly, $t = 0$ and $p = 1.0$.

### Your Task

Implement \`ttest_one_sample(data, mu)\` that prints the $t$-statistic (rounded to 4 decimal places) and whether the result is significant ($p < 0.05$).`,

	starterCode: `import math, statistics

def ttest_one_sample(data, mu):
    # Print t_stat (round 4) and whether p < 0.05 (True/False)
    pass

ttest_one_sample([1, 2, 3, 4, 5], 3)
`,

	solution: `import math, statistics

def _betacf(a, b, x):
    MAXIT, EPS = 100, 3e-7
    qab, qap, qam = a+b, a+1, a-1
    c, d = 1.0, max(1-qab*x/qap, 1e-30)
    d, h = 1.0/d, 1.0/d
    for m in range(1, MAXIT+1):
        m2 = 2*m
        aa = m*(b-m)*x/((qam+m2)*(a+m2))
        d = max(1+aa*d, 1e-30); c = max(1+aa/c, 1e-30)
        d = 1.0/d; h *= d*c
        aa = -(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d = max(1+aa*d, 1e-30); c = max(1+aa/c, 1e-30)
        d = 1.0/d; delta = d*c; h *= delta
        if abs(delta-1.0) < EPS: break
    return h

def _betainc(a, b, x):
    if x <= 0: return 0.0
    if x >= 1: return 1.0
    lbeta = math.lgamma(a)+math.lgamma(b)-math.lgamma(a+b)
    front = math.exp(math.log(x)*a + math.log(1-x)*b - lbeta)
    if x < (a+1)/(a+b+2): return front*_betacf(a,b,x)/a
    return 1 - front*_betacf(b,a,1-x)/b

def _t_pvalue(t, df):
    x = df / (df + t*t)
    return _betainc(df/2, 0.5, x)

def ttest_one_sample(data, mu):
    n = len(data)
    mean = statistics.fmean(data)
    s = statistics.stdev(data)
    t_stat = (mean - mu) / (s / math.sqrt(n))
    p_value = _t_pvalue(t_stat, n - 1)
    print(round(t_stat, 4))
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
