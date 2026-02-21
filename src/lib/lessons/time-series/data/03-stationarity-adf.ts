import type { Lesson } from "../../types";

export const stationarityAdfLesson: Lesson = {
  id: "stationarity-adf",
  title: "Stationarity & ADF Test",
  chapterId: "autocorrelation",
  content: `## Stationarity & the ADF Test

A time series is **stationary** if its statistical properties (mean, variance) do not change over time. Most forecasting models assume stationarity.

### Variance-Ratio Test

A simple check: split the series in half and compare the variances of both halves. If the ratio is close to 1, the series may be stationary.

\`\`\`
is_stationary(xs, threshold=0.5):
    half = len(xs) // 2
    ratio = variance(xs[:half]) / variance(xs[half:])
    return |ratio - 1| < threshold
\`\`\`

### Augmented Dickey-Fuller (ADF) Statistic

The ADF test regresses the first-difference \`Δxs[t]\` on \`xs[t-1]\`:

\`\`\`
Δxs[t] = intercept + slope * xs[t-1] + ε[t]
\`\`\`

The t-statistic of \`slope\` is the ADF statistic. A very negative value (e.g., < -3) suggests stationarity.

### Task

Implement \`is_stationary(xs, threshold=0.5)\` and \`adf_statistic(xs)\`.
`,
  starterCode: `import math

def mean(xs):
    return sum(xs) / len(xs)

def variance(xs):
    m = mean(xs)
    return sum((x - m) ** 2 for x in xs) / len(xs)

def is_stationary(xs, threshold=0.5):
    # Compare variance of first half vs second half
    pass

def adf_statistic(xs):
    # Regress diff(xs) on xs[:-1], return t-statistic of slope
    pass
`,
  solution: `import math

def mean(xs):
    return sum(xs) / len(xs)

def variance(xs):
    m = mean(xs)
    return sum((x - m) ** 2 for x in xs) / len(xs)

def is_stationary(xs, threshold=0.5):
    n = len(xs)
    half = n // 2
    v1 = variance(xs[:half])
    v2 = variance(xs[half:])
    if v2 == 0:
        return v1 == 0
    ratio = v1 / v2
    return abs(ratio - 1.0) < threshold

def adf_statistic(xs):
    dxs = [xs[i+1] - xs[i] for i in range(len(xs)-1)]
    y = dxs
    x = xs[:-1]
    n = len(y)
    mx = mean(x)
    my = mean(y)
    cov_xy = sum((x[i] - mx) * (y[i] - my) for i in range(n)) / n
    var_x = variance(x)
    slope = cov_xy / var_x
    intercept = my - slope * mx
    resids = [y[i] - (intercept + slope * x[i]) for i in range(n)]
    s2 = sum(r**2 for r in resids) / max(n - 2, 1)
    if s2 == 0:
        return 0.0
    se = math.sqrt(s2 / (n * var_x))
    return slope / se
`,
  tests: [
    {
      name: "oscillating series is stationary",
      code: `{{FUNC}}\nxs = [0.5, -0.3, 0.4, -0.2, 0.6, -0.4, 0.3, -0.5, 0.4, -0.3, 0.5, -0.4]\nprint(is_stationary(xs, 0.5))`,
      expected: "True\n",
    },
    {
      name: "growing series is not stationary",
      code: `{{FUNC}}\nxs = [1.0, 2.5, 2.0, 3.5, 3.0, 5.0, 6.0, 7.5, 8.0, 10.0, 11.0, 13.0]\nprint(is_stationary(xs, 0.5))`,
      expected: "False\n",
    },
    {
      name: "adf_statistic for stationary series is very negative",
      code: `{{FUNC}}\nxs = [0.5, -0.3, 0.4, -0.2, 0.6, -0.4, 0.3, -0.5, 0.4, -0.3, 0.5, -0.4]\nprint(round(adf_statistic(xs), 4))`,
      expected: "-14.4254\n",
    },
    {
      name: "adf_statistic for trending series is near zero or positive",
      code: `{{FUNC}}\nxs = [1.0, 2.5, 2.0, 3.5, 3.0, 5.0, 6.0, 7.5, 8.0, 10.0, 11.0, 13.0]\nprint(round(adf_statistic(xs), 4))`,
      expected: "0.8436\n",
    },
  ],
};
