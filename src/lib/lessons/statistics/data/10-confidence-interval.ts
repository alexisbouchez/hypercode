import type { Lesson } from "../../types";

export const confidenceInterval: Lesson = {
	id: "confidence-interval",
	title: "Confidence Intervals",
	chapterId: "inference",
	content: `## Estimating the True Mean

A **confidence interval (CI)** gives a range of plausible values for the population mean, based on a sample.

\`\`\`python
import math, statistics

data = [1, 2, 3, 4, 5]
n = len(data)
mean = statistics.fmean(data)
sem = statistics.stdev(data) / math.sqrt(n)

# 95% CI: mean ± t_critical * sem
# t_critical for df=4, 95% ≈ 2.7764
t_crit = 2.7764
print(round(mean - t_crit * sem, 2))   # 1.04
print(round(mean + t_crit * sem, 2))   # 4.96
\`\`\`

### Interpretation

A 95% CI means: if you repeated this sampling process 100 times, approximately **95 of the resulting CIs** would contain the true population mean.

**Common misconception**: It does NOT mean "there's a 95% chance the true mean is in this interval." The true mean is fixed; the interval is random.

### Width of the CI

The CI gets narrower (more precise) when:
- Sample size **n** increases
- Data variability **σ** decreases
- Confidence level decreases (e.g., 90% CI is narrower than 95%)

### t vs z

We use the **t-distribution** (not normal) because we estimate σ from the sample. As n → ∞, the t-distribution approaches the normal distribution.

### Your Task

Implement \`confidence_interval(data, confidence)\` that prints the **lower bound** and **upper bound** of the confidence interval, each rounded to 2 decimal places.`,

	starterCode: `import math, statistics

def _t_ppf(p, df):
    # Inverse CDF of t-distribution via binary search
    # (uses _betainc and _t_pvalue — see solution for full helpers)
    pass

def confidence_interval(data, confidence):
    # Print lower and upper bounds of the CI, each rounded to 2 decimal places
    pass

confidence_interval([1, 2, 3, 4, 5], 0.95)
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

def _t_ppf(p, df):
    # Binary search for positive t where two-tailed p-value = 2*(1-p)
    lo, hi = 0.0, 100.0
    target = 2*(1-p)
    for _ in range(200):
        mid = (lo+hi)/2
        if _t_pvalue(mid, df) > target: lo = mid
        else: hi = mid
    return (lo+hi)/2

def confidence_interval(data, confidence):
    n = len(data)
    mean = statistics.fmean(data)
    sem = statistics.stdev(data) / math.sqrt(n)
    t_crit = _t_ppf((1 + confidence) / 2, n - 1)
    lo = mean - t_crit * sem
    hi = mean + t_crit * sem
    print(round(lo, 2))
    print(round(hi, 2))

confidence_interval([1, 2, 3, 4, 5], 0.95)
`,

	tests: [
		{
			name: "confidence_interval([1..5], 0.95) → [1.04, 4.96]",
			expected: "1.04\n4.96\n",
		},
		{
			name: "confidence_interval([10..50], 0.95) → [10.37, 49.63]",
			code: `{{FUNC}}
confidence_interval([10, 20, 30, 40, 50], 0.95)`,
			expected: "10.37\n49.63\n",
		},
		{
			name: "CI lower < mean < CI upper",
			code: `{{FUNC}}
data = [1, 2, 3, 4, 5]
mean = statistics.fmean(data)
n = len(data)
sem = statistics.stdev(data) / math.sqrt(n)
t_crit = _t_ppf(0.975, n - 1)
lo = mean - t_crit * sem
hi = mean + t_crit * sem
print(lo < mean < hi)`,
			expected: "True\n",
		},
		{
			name: "90% CI is narrower than 95% CI",
			code: `{{FUNC}}
data = [1, 2, 3, 4, 5]
n = len(data)
sem = statistics.stdev(data) / math.sqrt(n)
mean = statistics.fmean(data)
w90 = 2 * _t_ppf(0.95, n-1) * sem
w95 = 2 * _t_ppf(0.975, n-1) * sem
print(w90 < w95)`,
			expected: "True\n",
		},
	],
};
