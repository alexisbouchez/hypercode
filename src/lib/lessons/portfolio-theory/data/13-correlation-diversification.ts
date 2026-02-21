import type { Lesson } from "../../types";

export const correlationDiversification: Lesson = {
  id: "correlation-diversification",
  title: "Correlation & Diversification",
  chapterId: "factor-models",
  content: `## Correlation & Diversification

Diversification reduces risk — but by how much depends on the **correlation structure** between assets.

### Diversification Ratio

The **diversification ratio** compares the weighted average volatility of individual assets to the actual portfolio volatility:

$$DR = \\frac{\\sum_i w_i \\sigma_i}{\\sigma_p}$$

- DR = 1: no diversification benefit (all assets perfectly correlated)
- DR > 1: diversification reduces risk below the weighted average

### Effective Number of Assets

The **effective number of assets** (Herfindahl measure) captures portfolio concentration:

$$N_{\\text{eff}} = \\frac{1}{\\sum_i w_i^2}$$

- N_eff = N for equal weights (maximum diversification)
- N_eff = 1 for a single concentrated position

### Your Task

Implement:
- \`diversification_ratio(weights, sigmas, cov)\` — returns the diversification ratio; \`cov\` is a list-of-lists covariance matrix
- \`effective_n_assets(weights)\` — returns the effective number of assets`,
  starterCode: `import math

def diversification_ratio(weights, sigmas, cov):
    pass

def effective_n_assets(weights):
    pass`,
  solution: `import math

def portfolio_variance_n(weights, cov):
    n = len(weights)
    var = 0.0
    for i in range(n):
        for j in range(n):
            var += weights[i] * weights[j] * cov[i][j]
    return var

def diversification_ratio(weights, sigmas, cov):
    weighted_avg_vol = sum(w * s for w, s in zip(weights, sigmas))
    port_vol = math.sqrt(portfolio_variance_n(weights, cov))
    return weighted_avg_vol / port_vol

def effective_n_assets(weights):
    return 1.0 / sum(w**2 for w in weights)`,
  tests: [
    {
      name: "diversification ratio with partial correlation",
      code: `{{FUNC}}\ns1, s2, corr = 0.1, 0.2, 0.3\ncov = [[s1**2, corr*s1*s2], [corr*s1*s2, s2**2]]\nprint(round(diversification_ratio([0.5, 0.5], [s1, s2], cov), 4))`,
      expected: "1.2048\n",
    },
    {
      name: "effective number of assets (equal weights, 2 assets)",
      code: `{{FUNC}}\nprint(round(effective_n_assets([0.5, 0.5]), 4))`,
      expected: "2.0\n",
    },
    {
      name: "effective number of assets (equal weights, 3 assets)",
      code: `{{FUNC}}\nprint(round(effective_n_assets([1/3, 1/3, 1/3]), 4))`,
      expected: "3.0\n",
    },
    {
      name: "effective number of assets (concentrated portfolio)",
      code: `{{FUNC}}\nprint(round(effective_n_assets([0.9, 0.1]), 4))`,
      expected: "1.2195\n",
    },
  ],
};
