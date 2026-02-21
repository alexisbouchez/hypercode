import type { Lesson } from "../../types";

export const portfolioVariance2Asset: Lesson = {
  id: "portfolio-variance-2asset",
  title: "Portfolio Variance (2-Asset)",
  chapterId: "return-risk",
  content: `## Portfolio Variance for Two Assets

While the expected return of a portfolio is simply a weighted average, **portfolio variance** also depends on how the assets co-move — captured by their **correlation**.

For two assets with weights w₁, w₂ and standard deviations σ₁, σ₂:

$$\\sigma_p^2 = w_1^2 \\sigma_1^2 + w_2^2 \\sigma_2^2 + 2 w_1 w_2 \\rho_{12} \\sigma_1 \\sigma_2$$

where ρ₁₂ is the correlation between the two assets.

### Key Insight: Diversification

- If ρ = 1 (perfect positive correlation): no diversification benefit
- If ρ = 0 (uncorrelated): partial diversification
- If ρ = −1 (perfect negative correlation): maximum diversification — portfolio variance can reach **zero**

### Your Task

Implement:
- \`portfolio_variance_2(w1, s1, w2, s2, corr)\` — returns portfolio variance
- \`portfolio_std_2(w1, s1, w2, s2, corr)\` — returns portfolio standard deviation (square root of variance)

Use only the \`math\` module.`,
  starterCode: `import math

def portfolio_variance_2(w1, s1, w2, s2, corr):
    pass

def portfolio_std_2(w1, s1, w2, s2, corr):
    pass`,
  solution: `import math

def portfolio_variance_2(w1, s1, w2, s2, corr):
    return w1**2 * s1**2 + w2**2 * s2**2 + 2 * w1 * w2 * corr * s1 * s2

def portfolio_std_2(w1, s1, w2, s2, corr):
    return math.sqrt(portfolio_variance_2(w1, s1, w2, s2, corr))`,
  tests: [
    {
      name: "variance with positive correlation",
      code: `{{FUNC}}\nprint(round(portfolio_variance_2(0.5, 0.1, 0.5, 0.2, 0.3), 6))`,
      expected: "0.0155\n",
    },
    {
      name: "std with positive correlation",
      code: `{{FUNC}}\nprint(round(portfolio_std_2(0.5, 0.1, 0.5, 0.2, 0.3), 6))`,
      expected: "0.124499\n",
    },
    {
      name: "variance with zero correlation",
      code: `{{FUNC}}\nprint(round(portfolio_variance_2(0.5, 0.1, 0.5, 0.2, 0.0), 6))`,
      expected: "0.0125\n",
    },
    {
      name: "std with perfect negative correlation",
      code: `{{FUNC}}\nprint(round(portfolio_std_2(0.5, 0.15, 0.5, 0.15, -1.0), 6))`,
      expected: "0.0\n",
    },
  ],
};
