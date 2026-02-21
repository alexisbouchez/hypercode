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

### Your Task

Implement \`min_var_portfolio_2(s1, s2, corr)\` that returns the weight in asset 1 (w₁) for the minimum variance portfolio.`,
  starterCode: `def min_var_portfolio_2(s1, s2, corr):
    pass`,
  solution: `def min_var_portfolio_2(s1, s2, corr):
    num = s2**2 - s1 * s2 * corr
    den = s1**2 + s2**2 - 2 * s1 * s2 * corr
    return num / den`,
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
  ],
};
