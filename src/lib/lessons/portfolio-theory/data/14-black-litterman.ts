import type { Lesson } from "../../types";

export const blackLitterman: Lesson = {
  id: "black-litterman",
  title: "Black-Litterman Views",
  chapterId: "factor-models",
  content: `## Black-Litterman Model

The **Black-Litterman model** (1990) combines a market equilibrium prior with investor views to produce a posterior estimate of expected returns.

### The Framework

- **Prior**: Market equilibrium returns π (implied by market cap weights and a risk model)
- **Views**: An investor expresses a view that a linear combination of assets will return Q, with uncertainty Ω
- **Posterior**: The BL formula blends the prior and views using Bayes' theorem

### Simplified Formula

For a single view expressed as a vector P (portfolio weights for the view) and target return Q:

$$\\mu_{BL} = \\pi + \\tau \\Sigma P^T \\left(P \\tau \\Sigma P^T + \\Omega\\right)^{-1} (Q - P \\pi)$$

In our simplified version, we use the identity matrix for Σ (unit covariance), so:

$$\\mu_{BL,i} = \\pi_i + \\frac{\\tau P_i}{\\tau \\sum_j P_j^2 + \\Omega} (Q - P \\cdot \\pi)$$

where τ (tau) is a scalar controlling prior uncertainty (default = 0.05).

### Your Task

Implement \`bl_posterior_return(pi, omega, P, Q, tau=0.05)\` where:
- \`pi\` — list of prior expected returns
- \`omega\` — scalar view uncertainty
- \`P\` — list of view portfolio weights
- \`Q\` — scalar view expected return

Return a list of posterior expected returns, each rounded to 4 decimal places.`,
  starterCode: `def bl_posterior_return(pi, omega, P, Q, tau=0.05):
    pass`,
  solution: `def bl_posterior_return(pi, omega, P, Q, tau=0.05):
    n = len(pi)
    tSPt = tau * sum(p**2 for p in P)
    scalar = 1.0 / (tSPt + omega)
    view_diff = Q - sum(P[i] * pi[i] for i in range(n))
    result = [pi[i] + tau * P[i] * scalar * view_diff for i in range(n)]
    return [round(r, 4) for r in result]`,
  tests: [
    {
      name: "long-short view (asset 1 outperforms asset 2)",
      code: `{{FUNC}}\nresult = bl_posterior_return([0.08, 0.10], 0.001, [1.0, -1.0], 0.02)\nprint(result)`,
      expected: "[0.0998, 0.0802]\n",
    },
    {
      name: "long-only view (blend expected to return 10%)",
      code: `{{FUNC}}\nresult = bl_posterior_return([0.06, 0.09], 0.002, [0.5, 0.5], 0.10)\nprint(result)`,
      expected: "[0.0831, 0.1131]\n",
    },
    {
      name: "prior unchanged when view matches prior",
      code: `{{FUNC}}\nresult = bl_posterior_return([0.08, 0.08], 0.001, [1.0, 0.0], 0.08)\nprint(result)`,
      expected: "[0.08, 0.08]\n",
    },
    {
      name: "posterior has same length as prior",
      code: `{{FUNC}}\nresult = bl_posterior_return([0.08, 0.10], 0.001, [1.0, -1.0], 0.02)\nprint(len(result))`,
      expected: "2\n",
    },
  ],
};
