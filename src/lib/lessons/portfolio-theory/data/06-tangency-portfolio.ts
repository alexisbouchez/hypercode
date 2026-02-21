import type { Lesson } from "../../types";

export const tangencyPortfolio: Lesson = {
  id: "tangency-portfolio",
  title: "Sharpe Ratio Maximization (Tangency Portfolio)",
  chapterId: "optimization",
  content: `## Tangency Portfolio

The **tangency portfolio** is the risky portfolio that maximizes the **Sharpe ratio**:

$$SR = \\frac{\\mu_p - r_f}{\\sigma_p}$$

It is the point on the efficient frontier where a line from the risk-free rate is tangent to the frontier.

### Analytical Solution for Two Assets

To find the tangency weights, we solve the system of equations z = Σ⁻¹(μ − r_f · 1) and normalize:

$$z_i = (\\Sigma^{-1} \\boldsymbol{e})_i, \\quad w_i = \\frac{z_i}{\\sum_j z_j}$$

For two assets with covariance matrix Σ = [[σ₁², ρσ₁σ₂], [ρσ₁σ₂, σ₂²]]:

$$z_1 = \\frac{\\sigma_2^2 e_1 - \\rho \\sigma_1 \\sigma_2 e_2}{\\det(\\Sigma)}, \\quad z_2 = \\frac{\\sigma_1^2 e_2 - \\rho \\sigma_1 \\sigma_2 e_1}{\\det(\\Sigma)}$$

where eᵢ = μᵢ − r_f and det(Σ) = σ₁²σ₂² − (ρσ₁σ₂)².

### Your Task

Implement \`tangency_weights_2(mu1, mu2, s1, s2, corr, rf)\` that returns a tuple \`(w1, w2)\` of the tangency portfolio weights, each rounded to 4 decimal places.`,
  starterCode: `def tangency_weights_2(mu1, mu2, s1, s2, corr, rf):
    pass`,
  solution: `def tangency_weights_2(mu1, mu2, s1, s2, corr, rf):
    e1 = mu1 - rf
    e2 = mu2 - rf
    cov12 = corr * s1 * s2
    det = s1**2 * s2**2 - cov12**2
    z1 = (s2**2 * e1 - cov12 * e2) / det
    z2 = (s1**2 * e2 - cov12 * e1) / det
    total = z1 + z2
    w1 = z1 / total
    w2 = z2 / total
    return round(w1, 4), round(w2, 4)`,
  tests: [
    {
      name: "tangency weights with positive correlation",
      code: `{{FUNC}}\nprint(tangency_weights_2(0.1, 0.15, 0.12, 0.18, 0.3, 0.03))`,
      expected: "(0.5391, 0.4609)\n",
    },
    {
      name: "tangency weights with zero correlation",
      code: `{{FUNC}}\nprint(tangency_weights_2(0.08, 0.12, 0.10, 0.20, 0.0, 0.02))`,
      expected: "(0.7059, 0.2941)\n",
    },
    {
      name: "weights sum to 1",
      code: `{{FUNC}}\nw1, w2 = tangency_weights_2(0.1, 0.15, 0.12, 0.18, 0.3, 0.03)\nprint(round(w1 + w2, 4))`,
      expected: "1.0\n",
    },
    {
      name: "weights sum to 1 (zero correlation case)",
      code: `{{FUNC}}\nw1, w2 = tangency_weights_2(0.08, 0.12, 0.10, 0.20, 0.0, 0.02)\nprint(round(w1 + w2, 4))`,
      expected: "1.0\n",
    },
  ],
};
