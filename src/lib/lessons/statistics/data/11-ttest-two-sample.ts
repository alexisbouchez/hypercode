import type { Lesson } from "../../types";

export const ttestTwoSample: Lesson = {
	id: "ttest-two-sample",
	title: "Two-Sample t-Test",
	chapterId: "inference",
	content: `## Comparing Two Groups

The **two-sample t-test** checks whether two independent groups have different means.

**Hypotheses:**
- $H_0$: $\mu_1 = \mu_2$ (no difference between groups)
- $H_1$: $\mu_1 \neq \mu_2$

\`\`\`python
import math, statistics

group_a = [1, 2, 3, 4, 5]
group_b = [6, 7, 8, 9, 10]

n1, n2 = len(group_a), len(group_b)
m1, m2 = statistics.fmean(group_a), statistics.fmean(group_b)
v1, v2 = statistics.variance(group_a), statistics.variance(group_b)

# Welch's t-statistic
t_stat = (m1 - m2) / math.sqrt(v1/n1 + v2/n2)
print(round(t_stat, 4))   # -5.0
\`\`\`

### Welch's t-Test

Use Welch's formula (no equal-variance assumption) for the most robust results. The t-statistic is:

$$t = \frac{\bar{x}_1 - \bar{x}_2}{\sqrt{\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}}}$$

The degrees of freedom use the Welch-Satterthwaite equation:

$$df = \frac{\left(\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}\right)^2}{\frac{(s_1^2/n_1)^2}{n_1-1} + \frac{(s_2^2/n_2)^2}{n_2-1}}$$

### Effect Size vs Statistical Significance

A statistically significant result ($p < 0.05$) does not necessarily mean the difference is **practically important**. With large samples, even tiny differences become significant.

### Same Groups → t = 0

When both groups are identical, $t = 0$ and $p = 1.0$.

### Your Task

Implement \`ttest_independent(group_a, group_b)\` that prints the $t$-statistic (rounded to 4 decimal places) and whether the result is significant ($p < 0.05$).`,

	starterCode: `import math, statistics

def ttest_independent(group_a, group_b):
    # Print t_stat (round 4) and whether p < 0.05 (True/False)
    pass

ttest_independent([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])
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

def ttest_independent(group_a, group_b):
    n1, n2 = len(group_a), len(group_b)
    m1, m2 = statistics.fmean(group_a), statistics.fmean(group_b)
    v1, v2 = statistics.variance(group_a), statistics.variance(group_b)
    se = math.sqrt(v1/n1 + v2/n2)
    t_stat = (m1 - m2) / se
    df = (v1/n1 + v2/n2)**2 / ((v1/n1)**2/(n1-1) + (v2/n2)**2/(n2-1))
    p_value = _t_pvalue(t_stat, df)
    print(round(t_stat, 4))
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
n1 = n2 = 5
a, b = [1,2,3,4,5], [6,7,8,9,10]
m1, m2 = statistics.fmean(a), statistics.fmean(b)
v1, v2 = statistics.variance(a), statistics.variance(b)
se = math.sqrt(v1/n1 + v2/n2)
t1 = (m1 - m2) / se
t2 = (m2 - m1) / se
print(round(t1, 4))
print(round(t2, 4))`,
			expected: "-5.0\n5.0\n",
		},
		{
			name: "well-separated groups → always significant",
			code: `{{FUNC}}
n1 = n2 = 3
a, b = [1, 2, 3], [100, 101, 102]
m1, m2 = statistics.fmean(a), statistics.fmean(b)
v1, v2 = statistics.variance(a), statistics.variance(b)
se = math.sqrt(v1/n1 + v2/n2)
t_stat = (m1 - m2) / se
df = (v1/n1 + v2/n2)**2 / ((v1/n1)**2/(n1-1) + (v2/n2)**2/(n2-1))
p_val = _t_pvalue(t_stat, df)
print(p_val < 0.001)`,
			expected: "True\n",
		},
	],
};
