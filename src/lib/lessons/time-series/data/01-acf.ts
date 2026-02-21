import type { Lesson } from "../../types";

export const acfLesson: Lesson = {
  id: "acf",
  title: "Autocorrelation Function (ACF)",
  chapterId: "autocorrelation",
  content: `## Autocorrelation Function (ACF)

The **Autocorrelation Function (ACF)** measures how correlated a time series is with a lagged version of itself. It is the foundation of time series analysis.

For a time series \`xs\` of length \`n\` with mean \`μ\`, the ACF at lag \`k\` is:

\`\`\`
acf(xs, k) = Σ(xs[i] - μ)(xs[i+k] - μ) / Σ(xs[i] - μ)²
\`\`\`

where the sums run over valid indices. By definition, \`acf(xs, 0) = 1.0\`.

High ACF at lag 1 means each value is strongly correlated with the previous one. ACF decaying slowly indicates a non-stationary or AR process.

### Task

Implement \`acf(xs, lag)\` which returns the autocorrelation of the series at the given lag.

- For \`lag == 0\`, return \`1.0\`
- Otherwise use the formula above
`,
  starterCode: `def mean(xs):
    return sum(xs) / len(xs)

def acf(xs, lag):
    # Return autocorrelation at given lag
    # Hint: acf(xs, 0) = 1.0
    pass
`,
  solution: `def mean(xs):
    return sum(xs) / len(xs)

def acf(xs, lag):
    if lag == 0:
        return 1.0
    n = len(xs)
    m = mean(xs)
    numerator = sum((xs[i] - m) * (xs[i + lag] - m) for i in range(n - lag))
    denominator = sum((x - m) ** 2 for x in xs)
    return numerator / denominator
`,
  tests: [
    {
      name: "acf at lag 0 is 1.0",
      code: `{{FUNC}}\nprint(round(acf([1,2,3,4,5,6,7,8], 0), 4))`,
      expected: "1.0\n",
    },
    {
      name: "acf([1,2,3,4,5,6,7,8], lag=1)",
      code: `{{FUNC}}\nprint(round(acf([1,2,3,4,5,6,7,8], 1), 4))`,
      expected: "0.625\n",
    },
    {
      name: "acf([1,2,3,4,5,6,7,8], lag=2)",
      code: `{{FUNC}}\nprint(round(acf([1,2,3,4,5,6,7,8], 2), 4))`,
      expected: "0.2738\n",
    },
    {
      name: "acf([1,2,3,4,5,6,7,8], lag=3)",
      code: `{{FUNC}}\nprint(round(acf([1,2,3,4,5,6,7,8], 3), 4))`,
      expected: "-0.0298\n",
    },
  ],
};
