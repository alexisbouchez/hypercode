import type { Lesson } from "../../types";

export const minimumVariance: Lesson = {
  id: "minimum-variance",
  title: "Minimum Variance Portfolio",
  chapterId: "return-risk",
  content: `## Minimum Variance Portfolio

The **minimum variance portfolio** (MVP) is the portfolio that achieves the lowest possible variance for a given set of assets — regardless of expected return.

For two assets, the optimal weight in asset 1 that minimizes portfolio variance has a closed-form solution:

$$w_1^* = \\frac{\\sigma_2^2 - \\sigma_1 \\sigma_2 \\rho_{12}}{\\sigma_1^2 + \\sigma_2^2 - 2 \\sigma_1 \\sigma_2 \\rho_{12}}$$

And w₂ = 1 − w₁.

### Intuition

- The numerator captures how much of asset 2's risk can be offset by holding asset 1
- When ρ = 0, the formula simplifies: w₁ = σ₂² / (σ₁² + σ₂²) — you hold more of the lower-volatility asset

### N-Asset Generalization

For N assets, the minimum variance portfolio requires solving a constrained optimization. Given the covariance matrix Σ (an N×N matrix), the MVP weights are:

$$\\mathbf{w}^* = \\frac{\\Sigma^{-1} \\mathbf{1}}{\\mathbf{1}^T \\Sigma^{-1} \\mathbf{1}}$$

where **1** is a vector of ones. This can be implemented without matrix inversion using a system of linear equations.

For the simple case of **diagonal covariance** (uncorrelated assets), this simplifies to inverse-variance weighting:

\`\`\`
w_i = (1 / sigma_i^2) / sum(1 / sigma_j^2 for all j)
\`\`\`

### Your Task

Implement:
- \`min_var_portfolio_2(s1, s2, corr)\` — returns w₁ for the 2-asset minimum variance portfolio
- \`min_var_n_uncorrelated(sigmas)\` — returns a list of minimum variance weights for N uncorrelated assets (inverse-variance weighting), each rounded to 4 decimal places`,
  starterCode: `def min_var_portfolio_2(s1, s2, corr):
    pass

def min_var_n_uncorrelated(sigmas):
    pass`,
  solution: `def min_var_portfolio_2(s1, s2, corr):
    num = s2**2 - s1 * s2 * corr
    den = s1**2 + s2**2 - 2 * s1 * s2 * corr
    return num / den

def min_var_n_uncorrelated(sigmas):
    inv_var = [1.0 / (s ** 2) for s in sigmas]
    total = sum(inv_var)
    return [round(v / total, 4) for v in inv_var]`,
  tests: [
    {
      name: "positive correlation",
      code: `{{FUNC}}\nprint(round(min_var_portfolio_2(0.1, 0.2, 0.3), 4))`,
      expected: "0.8947\n",
    },
    {
      name: "high positive correlation",
      code: `{{FUNC}}\nprint(round(min_var_portfolio_2(0.15, 0.25, 0.5), 4))`,
      expected: "0.9211\n",
    },
    {
      name: "zero correlation",
      code: `{{FUNC}}\nprint(round(min_var_portfolio_2(0.1, 0.2, 0.0), 4))`,
      expected: "0.8\n",
    },
    {
      name: "negative correlation, equal sigmas",
      code: `{{FUNC}}\nprint(round(min_var_portfolio_2(0.1, 0.1, -0.5), 4))`,
      expected: "0.5\n",
    },
    {
      name: "N-asset uncorrelated: 3 assets with different vols",
      code: `{{FUNC}}\nprint(min_var_n_uncorrelated([0.1, 0.2, 0.4]))`,
      expected: "[0.7619, 0.1905, 0.0476]\n",
    },
    {
      name: "N-asset uncorrelated: equal vols => equal weights",
      code: `{{FUNC}}\nw = min_var_n_uncorrelated([0.15, 0.15, 0.15])\nprint([round(x, 4) for x in w])`,
      expected: "[0.3333, 0.3333, 0.3333]\n",
    },
  ],
};
