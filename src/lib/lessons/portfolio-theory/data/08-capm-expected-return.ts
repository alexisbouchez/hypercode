import type { Lesson } from "../../types";

export const capmExpectedReturn: Lesson = {
  id: "capm-expected-return",
  title: "CAPM Beta & Expected Return",
  chapterId: "optimization",
  content: `## CAPM: Capital Asset Pricing Model

The **Capital Asset Pricing Model (CAPM)** provides a framework for pricing individual assets based on their systematic risk.

### Beta

**Beta** (β) measures an asset's sensitivity to market movements:

$$\\beta_i = \\frac{\\text{Cov}(R_i, R_m)}{\\text{Var}(R_m)}$$

- β = 1: asset moves in lockstep with the market
- β > 1: asset amplifies market movements (aggressive)
- β < 1: asset dampens market movements (defensive)

### CAPM Expected Return

The CAPM predicts an asset's expected return based on its beta:

$$E[R_i] = r_f + \\beta_i (E[R_m] - r_f)$$

where (E[R_m] − r_f) is the **market risk premium**.

### Your Task

Implement:
- \`capm_return(rf, beta, mu_m)\` — returns the CAPM expected return for an asset
- \`capm_beta(cov_im, var_m)\` — returns beta given the covariance of the asset with the market and the market variance`,
  starterCode: `def capm_return(rf, beta, mu_m):
    pass

def capm_beta(cov_im, var_m):
    pass`,
  solution: `def capm_return(rf, beta, mu_m):
    return rf + beta * (mu_m - rf)

def capm_beta(cov_im, var_m):
    return cov_im / var_m`,
  tests: [
    {
      name: "CAPM return for high-beta asset",
      code: `{{FUNC}}\nprint(round(capm_return(0.03, 1.2, 0.10), 4))`,
      expected: "0.114\n",
    },
    {
      name: "CAPM return for low-beta asset",
      code: `{{FUNC}}\nprint(round(capm_return(0.03, 0.8, 0.10), 4))`,
      expected: "0.086\n",
    },
    {
      name: "beta from covariance and variance",
      code: `{{FUNC}}\nprint(round(capm_beta(0.006, 0.04), 4))`,
      expected: "0.15\n",
    },
    {
      name: "low beta from covariance and variance",
      code: `{{FUNC}}\nprint(round(capm_beta(0.002, 0.04), 4))`,
      expected: "0.05\n",
    },
  ],
};
