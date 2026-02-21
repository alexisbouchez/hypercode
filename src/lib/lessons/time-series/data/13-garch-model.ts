import type { Lesson } from "../../types";

export const garchModelLesson: Lesson = {
  id: "garch-model",
  title: "GARCH(1,1) Model",
  chapterId: "volatility-models",
  content: `## GARCH(1,1) Model

The **GARCH(1,1)** (Generalized ARCH) model by Bollerslev (1986) extends ARCH by also incorporating lagged conditional variances:

\`\`\`
h[t] = ω + α · r[t-1]² + β · h[t-1]
\`\`\`

where:
- \`h[t]\` = conditional variance at time t
- \`ω\` = base constant (omega > 0)
- \`α\` = ARCH coefficient (sensitivity to recent shocks)
- \`β\` = GARCH coefficient (persistence of variance)
- Stationarity requires \`α + β < 1\`

The initial variance \`h[0]\` is set to the sample variance of the returns.

GARCH(1,1) is the workhorse volatility model in finance — it captures both volatility clustering and mean reversion in variance.

### Task

Implement \`garch_variance(returns, omega, alpha, beta_g)\` that returns the list of conditional variances.
`,
  starterCode: `def garch_variance(returns, omega, alpha, beta_g):
    # h[0] = sample variance of returns
    # h[t] = omega + alpha * returns[t-1]^2 + beta_g * h[t-1]
    pass
`,
  solution: `def garch_variance(returns, omega, alpha, beta_g):
    n = len(returns)
    mean_r = sum(returns) / n
    h0 = sum((r - mean_r)**2 for r in returns) / n
    h = [h0]
    for t in range(1, n):
        h.append(omega + alpha * returns[t-1]**2 + beta_g * h[-1])
    return h
`,
  tests: [
    {
      name: "garch_variance initial variance",
      code: `{{FUNC}}\nreturns = [0.1, -0.2, 0.15, -0.05, 0.2]\nprint(round(garch_variance(returns, 0.01, 0.1, 0.8)[0], 6))`,
      expected: "0.0214\n",
    },
    {
      name: "garch_variance full sequence",
      code: `{{FUNC}}\nreturns = [0.1, -0.2, 0.15, -0.05, 0.2]\nprint([round(v, 6) for v in garch_variance(returns, 0.01, 0.1, 0.8)])`,
      expected: "[0.0214, 0.02812, 0.036496, 0.041447, 0.043407]\n",
    },
    {
      name: "garch_variance omega=0.005 alpha=0.15 beta=0.75",
      code: `{{FUNC}}\nreturns = [0.05, -0.1, 0.08, -0.15, 0.1]\nprint([round(v, 6) for v in garch_variance(returns, 0.005, 0.15, 0.75)])`,
      expected: "[0.010264, 0.013073, 0.016305, 0.018189, 0.022016]\n",
    },
    {
      name: "garch variance increases after large return",
      code: `{{FUNC}}\nreturns = [0.01, 0.5, 0.01, 0.01, 0.01]\nh = garch_variance(returns, 0.01, 0.2, 0.7)\nprint(h[2] > h[1])`,
      expected: "True\n",
    },
  ],
};
