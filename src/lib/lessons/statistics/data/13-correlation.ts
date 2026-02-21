import type { Lesson } from "../../types";

export const correlation: Lesson = {
	id: "correlation",
	title: "Correlation",
	chapterId: "regression",
	content: `## Measuring Linear Relationships

**Pearson's r** measures the strength and direction of a linear relationship between two variables. It ranges from $-1$ to $+1$.

$$r = \frac{\sum_{i=1}^n (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i=1}^n (x_i - \bar{x})^2 \sum_{i=1}^n (y_i - \bar{y})^2}}$$

\`\`\`python
def pearson_r(x, y):
    n = len(x)
    mx = sum(x) / n
    my = sum(y) / n
    num = sum((xi - mx) * (yi - my) for xi, yi in zip(x, y))
    den = (sum((xi - mx)**2 for xi in x) * sum((yi - my)**2 for yi in y)) ** 0.5
    return num / den

x = [1, 2, 3, 4, 5]
y = [2, 4, 6, 8, 10]   # perfect positive relationship

r = pearson_r(x, y)
print(round(r, 4))   # 1.0
\`\`\`

### Interpreting r

| $r$ value | Interpretation |
|---------|---------------|
| $r = 1.0$ | Perfect positive linear relationship |
| $r = 0.7$ | Strong positive |
| $r = 0.3$ | Weak positive |
| $r = 0.0$ | No linear relationship |
| $r = -0.7$ | Strong negative |
| $r = -1.0$ | Perfect negative linear relationship |

### Correlation does not imply Causation

A high correlation between $X$ and $Y$ does not mean $X$ causes $Y$. There may be a confounding variable, or the relationship may be coincidental.

### Significance

The $t$-statistic

$$t = r\sqrt{\frac{n-2}{1-r^2}}$$

follows a $t$-distribution with $df = n - 2$, allowing us to test if $r$ is significantly different from 0.

### Your Task

Implement \`pearson_r(x, y)\` that prints the correlation coefficient $r$ (rounded to 4 decimal places) and whether the relationship is statistically significant ($p < 0.001$).`,

	starterCode: `import math

def pearson_r(x, y):
    # Print r (round 4) and whether p < 0.001 (True/False)
    pass

pearson_r([1, 2, 3, 4, 5], [2, 4, 6, 8, 10])
`,

	solution: `import math

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

def pearson_r(x, y):
    n = len(x)
    mx = sum(x) / n
    my = sum(y) / n
    num = sum((xi - mx) * (yi - my) for xi, yi in zip(x, y))
    den = (sum((xi - mx)**2 for xi in x) * sum((yi - my)**2 for yi in y)) ** 0.5
    r = num / den
    t_stat = r * math.sqrt((n - 2) / (1 - r**2)) if abs(r) < 1 else float('inf')
    p = _t_pvalue(t_stat, n - 2)
    print(round(r, 4))
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
x, y = [1, 2, 3, 4, 5], [3, 1, 4, 1, 5]
n = len(x)
mx, my = sum(x)/n, sum(y)/n
num = sum((xi-mx)*(yi-my) for xi,yi in zip(x,y))
den = (sum((xi-mx)**2 for xi in x) * sum((yi-my)**2 for yi in y)) ** 0.5
r = num / den
t_stat = r * math.sqrt((n-2)/(1-r**2)) if abs(r) < 1 else float('inf')
p = _t_pvalue(t_stat, n-2)
print(abs(r) < 0.5)
print(p > 0.05)`,
			expected: "True\nTrue\n",
		},
		{
			name: "r is between -1 and 1",
			code: `{{FUNC}}
x, y = [1, 3, 2, 5, 4], [2, 5, 3, 8, 6]
n = len(x)
mx, my = sum(x)/n, sum(y)/n
num = sum((xi-mx)*(yi-my) for xi,yi in zip(x,y))
den = (sum((xi-mx)**2 for xi in x) * sum((yi-my)**2 for yi in y)) ** 0.5
r = num / den
print(-1.0 <= r <= 1.0)`,
			expected: "True\n",
		},
	],
};
