import type { Lesson } from "../../types";

export const pacfLesson: Lesson = {
  id: "pacf",
  title: "Partial Autocorrelation (PACF)",
  chapterId: "autocorrelation",
  content: `## Partial Autocorrelation (PACF)

The **Partial Autocorrelation Function (PACF)** at lag \`k\` measures the correlation between \`xs[t]\` and \`xs[t-k]\` after removing the effects of all intermediate lags.

PACF is computed using the **Levinson-Durbin recursion** on the Yule-Walker equations:

1. \`φ[1][1] = acf(xs, 1)\`
2. For each \`k ≥ 2\`:
   - \`φ[k][k] = (acf(k) - Σ φ[k-1][j] * acf(k-j)) / (1 - Σ φ[k-1][j] * acf(j))\`
   - \`φ[k][j] = φ[k-1][j] - φ[k][k] * φ[k-1][k-j]\` for \`j = 1..k-1\`
3. \`pacf(xs, k) = φ[k][k]\`

PACF cuts off at the AR order — a PACF that drops to zero after lag \`p\` suggests an AR(p) model.

### Task

Implement \`pacf(xs, lag)\` using the Levinson-Durbin recursion.

- \`pacf(xs, 0) = 1.0\`
- \`pacf(xs, 1) = acf(xs, 1)\`
- For higher lags, use the Yule-Walker approach above
`,
  starterCode: `def mean(xs):
    return sum(xs) / len(xs)

def acf(xs, lag):
    if lag == 0:
        return 1.0
    n = len(xs)
    m = mean(xs)
    numerator = sum((xs[i] - m) * (xs[i + lag] - m) for i in range(n - lag))
    denominator = sum((x - m) ** 2 for x in xs)
    return numerator / denominator

def pacf(xs, lag):
    # Use Levinson-Durbin recursion
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

def pacf(xs, lag):
    if lag == 0:
        return 1.0
    if lag == 1:
        return acf(xs, 1)
    r = [acf(xs, k) for k in range(lag + 1)]
    phi = [[0.0] * (lag + 1) for _ in range(lag + 1)]
    phi[1][1] = r[1]
    for k in range(2, lag + 1):
        num = r[k] - sum(phi[k-1][j] * r[k-j] for j in range(1, k))
        den = 1.0 - sum(phi[k-1][j] * r[j] for j in range(1, k))
        phi[k][k] = num / den
        for j in range(1, k):
            phi[k][j] = phi[k-1][j] - phi[k][k] * phi[k-1][k-j]
    return phi[lag][lag]
`,
  tests: [
    {
      name: "pacf at lag 1 equals acf at lag 1",
      code: `{{FUNC}}\nprint(round(pacf([1,2,3,4,5,6,7,8,9,10], 1), 4))`,
      expected: "0.7\n",
    },
    {
      name: "pacf([1..10], lag=2)",
      code: `{{FUNC}}\nprint(round(pacf([1,2,3,4,5,6,7,8,9,10], 2), 4))`,
      expected: "-0.1527\n",
    },
    {
      name: "pacf([1..10], lag=3)",
      code: `{{FUNC}}\nprint(round(pacf([1,2,3,4,5,6,7,8,9,10], 3), 4))`,
      expected: "-0.1549\n",
    },
    {
      name: "pacf at lag 0 is 1.0",
      code: `{{FUNC}}\nprint(round(pacf([1,2,3,4,5,6,7,8,9,10], 0), 4))`,
      expected: "1.0\n",
    },
  ],
};
