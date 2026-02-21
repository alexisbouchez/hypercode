import type { Lesson } from "../../types";

export const bootstrap: Lesson = {
	id: "bootstrap",
	title: "Bootstrap Resampling",
	chapterId: "regression",
	content: `## Resampling to Estimate Uncertainty

**Bootstrap resampling** is a powerful technique to estimate the uncertainty of any statistic — no mathematical formulas required.

The idea: repeatedly sample your data **with replacement**, compute your statistic each time, and look at the distribution of results.

\`\`\`python
import random

data = [1, 2, 3, 4, 5]
rng = random.Random(42)

# Draw 1000 bootstrap samples and compute means
boot_means = [
    sum(rng.choices(data, k=len(data))) / len(data)
    for _ in range(1000)
]
boot_means.sort()

# 95% bootstrap confidence interval (percentile method)
def pct(p):
    i = (p / 100) * (len(boot_means) - 1)
    lo = int(i)
    hi = min(lo + 1, len(boot_means) - 1)
    return boot_means[lo] + (i - lo) * (boot_means[hi] - boot_means[lo])

print(round(pct(2.5), 1), round(pct(97.5), 1))
\`\`\`

### Why Bootstrap?

- Works for **any statistic** (median, $\sigma$, correlation, custom metrics)
- No assumptions about the underlying distribution
- Especially useful for small samples or unusual statistics

### With Replacement

Sampling **with replacement** means each draw comes from the full original dataset. Some values appear multiple times, others don't appear. This mimics the randomness of collecting new samples.

### Percentile Method

The **percentile method** extracts the CI directly from the bootstrap distribution:
- Lower bound: 2.5th percentile of bootstrap means
- Upper bound: 97.5th percentile of bootstrap means

The 95% bootstrap CI is $[\hat{\theta}_{2.5\%},\, \hat{\theta}_{97.5\%}]$, where $\hat{\theta}$ is the statistic computed on each bootstrap sample.

### Your Task

Implement \`bootstrap_ci(data, n_samples, seed)\` that returns a tuple \`(lower, upper)\` representing the **95% bootstrap confidence interval** of the mean, rounded to 2 decimal places.`,

	starterCode: `import random

def bootstrap_ci(data, n_samples, seed):
    # Resample with replacement n_samples times, compute means
    # Return (lower, upper) for 95% CI (2.5th and 97.5th percentiles), rounded to 2
    pass

lower, upper = bootstrap_ci([1, 2, 3, 4, 5], 1000, 42)
print(lower < 3.0 < upper)   # True: CI should contain the mean
`,

	solution: `import random

def bootstrap_ci(data, n_samples, seed):
    rng = random.Random(seed)
    n = len(data)
    boot_means = sorted(
        sum(rng.choices(data, k=n)) / n
        for _ in range(n_samples)
    )
    def pct(p):
        i = (p / 100) * (n_samples - 1)
        lo = int(i)
        hi = min(lo + 1, n_samples - 1)
        return boot_means[lo] + (i - lo) * (boot_means[hi] - boot_means[lo])
    lower = round(pct(2.5), 2)
    upper = round(pct(97.5), 2)
    return lower, upper

lower, upper = bootstrap_ci([1, 2, 3, 4, 5], 1000, 42)
print(lower < 3.0 < upper)   # True: CI should contain the mean
`,

	tests: [
		{
			name: "95% CI of [1..5] contains the mean (3.0)",
			expected: "True\n",
		},
		{
			name: "CI lower < CI upper",
			code: `{{FUNC}}
lower, upper = bootstrap_ci([1, 2, 3, 4, 5], 1000, 42)
print(lower < upper)`,
			expected: "True\n",
		},
		{
			name: "CI bounds are within data range",
			code: `{{FUNC}}
lower, upper = bootstrap_ci([1, 2, 3, 4, 5], 1000, 42)
print(1.0 <= lower and upper <= 5.0)`,
			expected: "True\n",
		},
		{
			name: "CI of constant data is a single point",
			code: `{{FUNC}}
lower, upper = bootstrap_ci([7, 7, 7, 7, 7], 100, 0)
print(lower == 7.0)
print(upper == 7.0)`,
			expected: "True\nTrue\n",
		},
	],
};
