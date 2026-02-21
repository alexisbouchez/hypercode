import type { Lesson } from "../../types";

export const tTest: Lesson = {
  id: "t-test",
  title: "Hypothesis Testing (t-test)",
  chapterId: "statistical-testing",
  content: `## Hypothesis Testing: t-test

A **t-test** tests whether an observed difference is statistically significant or could be due to random chance.

**One-sample t-statistic** tests whether a sample mean differs from a hypothesized value $\\mu_0$:
$$t = \\frac{\\bar{x} - \\mu_0}{s / \\sqrt{n}}$$

where $s$ is the sample standard deviation and $n$ is the sample size. Large $|t|$ values indicate the sample mean is unlikely to come from a distribution with mean $\\mu_0$.

**Two-sample t-statistic** (Welch's) tests whether two independent samples have different means:
$$t = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}}}$$

Welch's version is preferred because it does not assume equal variances.

### Your Task

Implement:
- \`t_statistic(sample, mu0)\` — one-sample t-statistic
- \`two_sample_t(xs, ys)\` — Welch's two-sample t-statistic`,
  starterCode: `import math

def t_statistic(sample, mu0):
    # (mean - mu0) / (sample_std / sqrt(n))
    pass

def two_sample_t(xs, ys):
    # (mean_x - mean_y) / sqrt(var_x/n_x + var_y/n_y)
    pass`,
  solution: `import math

def t_statistic(sample, mu0):
    n = len(sample)
    m = sum(sample) / n
    std = math.sqrt(sum((x - m) ** 2 for x in sample) / (n - 1))
    return (m - mu0) / (std / math.sqrt(n))

def two_sample_t(xs, ys):
    n1, n2 = len(xs), len(ys)
    m1 = sum(xs) / n1
    m2 = sum(ys) / n2
    s1sq = sum((x - m1) ** 2 for x in xs) / (n1 - 1)
    s2sq = sum((y - m2) ** 2 for y in ys) / (n2 - 1)
    return (m1 - m2) / math.sqrt(s1sq / n1 + s2sq / n2)`,
  tests: [
    {
      name: "t_statistic for sample near mu0=2.0 equals ~2.6112",
      code: `{{FUNC}}
sample = [2.1, 2.3, 2.0, 2.4, 2.2, 1.9, 2.5, 2.1]
print(round(t_statistic(sample, 2.0), 4))`,
      expected: "2.6112\n",
    },
    {
      name: "two_sample_t([1,2,3,4,5], [2,3,2,4,3]) equals 0.25",
      code: `{{FUNC}}
print(round(two_sample_t([1,2,3,4,5], [2,3,2,4,3]), 4))`,
      expected: "0.25\n",
    },
    {
      name: "t_statistic equals 0.0 when sample mean equals mu0",
      code: `{{FUNC}}
print(round(t_statistic([1.0, 2.0, 3.0], 2.0), 4))`,
      expected: "0.0\n",
    },
  ],
};
