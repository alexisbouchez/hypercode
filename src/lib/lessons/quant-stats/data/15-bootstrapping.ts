import type { Lesson } from "../../types";

export const bootstrapping: Lesson = {
  id: "bootstrapping",
  title: "Bootstrapping Returns",
  chapterId: "regression",
  content: `## Bootstrapping Returns

**Bootstrapping** is a resampling technique to estimate the sampling distribution of a statistic without making distributional assumptions. It is essential in finance for constructing confidence intervals on Sharpe ratios, means, and other metrics.

The procedure:
1. From a sample of $n$ values, draw $n$ values **with replacement** → bootstrap sample
2. Compute the statistic of interest on this bootstrap sample
3. Repeat $B$ times to get a bootstrap distribution
4. Use percentiles of this distribution for confidence intervals

**Bootstrap confidence interval** at level $\\alpha$ (e.g., 0.05 for 95% CI):
- Lower bound: $\\alpha/2$ percentile of bootstrap distribution
- Upper bound: $(1 - \\alpha/2)$ percentile

Use \`random.choices(xs, k=len(xs))\` for sampling with replacement.

### Your Task

Implement:
- \`bootstrap_mean(xs, n_boot, seed)\` — list of bootstrap sample means
- \`bootstrap_ci(xs, n_boot, seed, alpha=0.05)\` — \`(lower, upper)\` confidence interval`,
  starterCode: `import random

def bootstrap_mean(xs, n_boot, seed):
    # Return list of n_boot bootstrap sample means
    # Use random.seed(seed), then random.choices for each replicate
    pass

def bootstrap_ci(xs, n_boot, seed, alpha=0.05):
    # Sort bootstrap means and return (lower, upper) percentile bounds
    pass`,
  solution: `import random

def bootstrap_mean(xs, n_boot, seed):
    random.seed(seed)
    means = []
    for _ in range(n_boot):
        sample = random.choices(xs, k=len(xs))
        means.append(sum(sample) / len(sample))
    return means

def bootstrap_ci(xs, n_boot, seed, alpha=0.05):
    means = bootstrap_mean(xs, n_boot, seed)
    means.sort()
    lower_idx = int(alpha / 2 * n_boot)
    upper_idx = int((1 - alpha / 2) * n_boot) - 1
    return (means[lower_idx], means[upper_idx])`,
  tests: [
    {
      name: "bootstrap_mean returns 1000 values",
      code: `{{FUNC}}
bm = bootstrap_mean([1,2,3,4,5,6,7,8,9,10], 1000, 42)
print(len(bm))`,
      expected: "1000\n",
    },
    {
      name: "bootstrap_ci([1..10], 1000, 42) gives (3.8, 7.2)",
      code: `{{FUNC}}
lower, upper = bootstrap_ci([1,2,3,4,5,6,7,8,9,10], 1000, 42)
print(round(lower, 4))
print(round(upper, 4))`,
      expected: "3.8\n7.2\n",
    },
    {
      name: "bootstrap CI contains the true mean",
      code: `{{FUNC}}
xs = [1,2,3,4,5,6,7,8,9,10]
true_mean = sum(xs) / len(xs)
lower, upper = bootstrap_ci(xs, 1000, 42)
print(lower <= true_mean <= upper)`,
      expected: "True\n",
    },
  ],
};
