import type { Lesson } from "../../types";

export const covarianceMatrix: Lesson = {
  id: "covariance-matrix",
  title: "N-Asset Covariance Matrix",
  chapterId: "return-risk",
  content: `## N-Asset Covariance Matrix

For a portfolio of N assets, the full variance-covariance structure is captured in an **N×N covariance matrix** Σ.

The (i, j) entry of Σ is the sample covariance between asset i and asset j:

$$\\Sigma_{ij} = \\frac{1}{T-1} \\sum_{t=1}^{T} (r_{i,t} - \\bar{r}_i)(r_{j,t} - \\bar{r}_j)$$

Note: when i = j this reduces to the variance of asset i.

### Portfolio Variance

Given weights vector **w** and covariance matrix Σ, portfolio variance is:

$$\\sigma_p^2 = \\mathbf{w}^T \\Sigma \\mathbf{w} = \\sum_i \\sum_j w_i w_j \\Sigma_{ij}$$

### Your Task

Implement:
- \`cov_matrix(returns_list)\` — takes a list of return series (each a list of T returns), returns an N×N covariance matrix as a list of lists (sample covariance, divide by T−1)
- \`portfolio_variance_n(weights, cov)\` — computes **w**ᵀ Σ **w** given a weights list and a covariance matrix (list of lists)`,
  starterCode: `def cov_matrix(returns_list):
    pass

def portfolio_variance_n(weights, cov):
    pass`,
  solution: `def cov_matrix(returns_list):
    n = len(returns_list)
    m = len(returns_list[0])
    means = [sum(returns_list[i]) / m for i in range(n)]
    cov = [[0.0] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            s = sum(
                (returns_list[i][k] - means[i]) * (returns_list[j][k] - means[j])
                for k in range(m)
            )
            cov[i][j] = s / (m - 1)
    return cov

def portfolio_variance_n(weights, cov):
    n = len(weights)
    var = 0.0
    for i in range(n):
        for j in range(n):
            var += weights[i] * weights[j] * cov[i][j]
    return var`,
  tests: [
    {
      name: "diagonal element (variance of asset 1)",
      code: `{{FUNC}}\nr1 = [0.1, 0.05, 0.08, 0.12, 0.03]\nr2 = [0.06, 0.09, 0.04, 0.07, 0.11]\ncov = cov_matrix([r1, r2])\nprint(round(cov[0][0], 6))`,
      expected: "0.00133\n",
    },
    {
      name: "off-diagonal element (covariance)",
      code: `{{FUNC}}\nr1 = [0.1, 0.05, 0.08, 0.12, 0.03]\nr2 = [0.06, 0.09, 0.04, 0.07, 0.11]\ncov = cov_matrix([r1, r2])\nprint(round(cov[0][1], 6))`,
      expected: "-0.00068\n",
    },
    {
      name: "2-asset portfolio variance",
      code: `{{FUNC}}\nr1 = [0.1, 0.05, 0.08, 0.12, 0.03]\nr2 = [0.06, 0.09, 0.04, 0.07, 0.11]\ncov = cov_matrix([r1, r2])\nprint(round(portfolio_variance_n([0.6, 0.4], cov), 6))`,
      expected: "0.000269\n",
    },
    {
      name: "3-asset portfolio variance",
      code: `{{FUNC}}\nr1 = [0.1, 0.05, 0.08, 0.12, 0.03]\nr2 = [0.06, 0.09, 0.04, 0.07, 0.11]\nr3 = [0.02, 0.03, 0.01, 0.04, 0.02]\ncov = cov_matrix([r1, r2, r3])\nprint(round(portfolio_variance_n([0.4, 0.4, 0.2], cov), 6))`,
      expected: "0.000157\n",
    },
  ],
};
