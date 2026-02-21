import type { Lesson } from "../../types";

export const parametricVar: Lesson = {
  id: "parametric-var",
  title: "Parametric VaR (Normal)",
  chapterId: "market-risk",
  content: `## Parametric VaR (Normal Distribution)

The **parametric** (variance-covariance) approach assumes returns follow a normal distribution. Given the mean \`μ\` and standard deviation \`σ\` of daily returns:

### Formula

\`\`\`
VaR = -(μ + z * σ)
\`\`\`

where \`z = N⁻¹(1 - confidence)\` is the inverse normal CDF quantile.

Key quantiles:
- 95% confidence → z ≈ −1.6449
- 99% confidence → z ≈ −2.3263

### Implementing the Inverse Normal CDF

Use the identity: \`N⁻¹(p) = √2 · erfinv(2p − 1)\`

Implement \`erfinv\` via bisection on \`math.erf\`.

### Example

μ = 0.001, σ = 0.02, 95% confidence:  
z = N⁻¹(0.05) ≈ −1.6449  
VaR = −(0.001 + (−1.6449)(0.02)) = −(0.001 − 0.03290) ≈ **0.0319**
`,
  starterCode: `import math

def erfinv(y, tol=1e-10):
    lo, hi = -10.0, 10.0
    for _ in range(100):
        mid = (lo + hi) / 2.0
        if math.erf(mid) < y:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2.0

def norm_ppf(p):
    pass

def parametric_var(mu, sigma, confidence=0.95):
    pass
`,
  solution: `import math

def erfinv(y, tol=1e-10):
    lo, hi = -10.0, 10.0
    for _ in range(100):
        mid = (lo + hi) / 2.0
        if math.erf(mid) < y:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2.0

def norm_ppf(p):
    return math.sqrt(2) * erfinv(2 * p - 1)

def parametric_var(mu, sigma, confidence=0.95):
    z = norm_ppf(1 - confidence)
    return -(mu + z * sigma)
`,
  tests: [
    {
      name: "parametric_var mu=0.001 sigma=0.02 at 95%",
      code: `{{FUNC}}\nprint(round(parametric_var(0.001, 0.02, 0.95), 4))`,
      expected: "0.0319\n",
    },
    {
      name: "parametric_var mu=0.0 sigma=0.01 at 99%",
      code: `{{FUNC}}\nprint(round(parametric_var(0.0, 0.01, 0.99), 4))`,
      expected: "0.0233\n",
    },
    {
      name: "parametric_var mu=0.002 sigma=0.03 at 95%",
      code: `{{FUNC}}\nprint(round(parametric_var(0.002, 0.03, 0.95), 4))`,
      expected: "0.0473\n",
    },
    {
      name: "norm_ppf at p=0.05 is approximately -1.6449",
      code: `{{FUNC}}\nprint(round(norm_ppf(0.05), 4))`,
      expected: "-1.6449\n",
    },
  ],
};
