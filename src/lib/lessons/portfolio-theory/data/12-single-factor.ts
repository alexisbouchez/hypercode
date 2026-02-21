import type { Lesson } from "../../types";

export const singleFactor: Lesson = {
  id: "single-factor",
  title: "Single-Factor Model",
  chapterId: "capital-market-theory",
  content: `## Single-Factor Model (OLS Regression)

The single-factor model explains an asset's excess return as a linear function of a single factor's excess return:

$$R_i - r_f = \\alpha + \\beta (R_f^{\\text{factor}} - r_f) + \\varepsilon$$

This is estimated via **Ordinary Least Squares (OLS)** regression.

### OLS Formulas

Given two series of observations (x, y), the OLS estimates are:

$$\\beta = \\frac{\\sum_t (x_t - \\bar{x})(y_t - \\bar{y})}{\\sum_t (x_t - \\bar{x})^2}$$

$$\\alpha = \\bar{y} - \\beta \\bar{x}$$

where we regress excess asset returns (y) on excess factor returns (x).

### Application

The **factor exposure** (beta) tells us how much the asset co-moves with the factor per unit of factor return. The **alpha** is the return unexplained by the factor.

### Your Task

Implement:
- \`factor_exposure(asset_returns, factor_returns)\` — OLS beta of asset returns on factor returns
- \`fama_french_alpha(asset_returns, factor_returns, rf)\` — alpha from regression of excess asset returns on excess factor returns`,
  starterCode: `def factor_exposure(asset_returns, factor_returns):
    pass

def fama_french_alpha(asset_returns, factor_returns, rf):
    pass`,
  solution: `def factor_exposure(asset_returns, factor_returns):
    n = len(asset_returns)
    mean_a = sum(asset_returns) / n
    mean_f = sum(factor_returns) / n
    cov = sum(
        (asset_returns[i] - mean_a) * (factor_returns[i] - mean_f)
        for i in range(n)
    )
    var_f = sum((factor_returns[i] - mean_f)**2 for i in range(n))
    return cov / var_f

def fama_french_alpha(asset_returns, factor_returns, rf):
    excess_a = [r - rf for r in asset_returns]
    excess_f = [r - rf for r in factor_returns]
    beta = factor_exposure(excess_a, excess_f)
    n = len(excess_a)
    mean_ea = sum(excess_a) / n
    mean_ef = sum(excess_f) / n
    return mean_ea - beta * mean_ef`,
  tests: [
    {
      name: "factor exposure (beta)",
      code: `{{FUNC}}\nasset_r = [0.10, 0.05, 0.12, 0.08, 0.11]\nfactor_r = [0.08, 0.04, 0.10, 0.06, 0.09]\nprint(round(factor_exposure(asset_r, factor_r), 4))`,
      expected: "1.1466\n",
    },
    {
      name: "Fama-French alpha",
      code: `{{FUNC}}\nasset_r = [0.10, 0.05, 0.12, 0.08, 0.11]\nfactor_r = [0.08, 0.04, 0.10, 0.06, 0.09]\nprint(round(fama_french_alpha(asset_r, factor_r, 0.02), 4))`,
      expected: "0.0101\n",
    },
    {
      name: "unit factor exposure",
      code: `{{FUNC}}\nasset_r2 = [0.06, 0.08, 0.07, 0.09, 0.05]\nfactor_r2 = [0.04, 0.06, 0.05, 0.07, 0.03]\nprint(round(factor_exposure(asset_r2, factor_r2), 4))`,
      expected: "1.0\n",
    },
    {
      name: "Fama-French alpha second case",
      code: `{{FUNC}}\nasset_r2 = [0.06, 0.08, 0.07, 0.09, 0.05]\nfactor_r2 = [0.04, 0.06, 0.05, 0.07, 0.03]\nprint(round(fama_french_alpha(asset_r2, factor_r2, 0.01), 4))`,
      expected: "0.02\n",
    },
  ],
};
