import type { Lesson } from "../../types";

export const riskDecomposition: Lesson = {
  id: "risk-decomposition",
  title: "Risk Decomposition",
  chapterId: "capital-market-theory",
  content: `## Risk Decomposition: Systematic vs. Idiosyncratic

CAPM decomposes total asset risk into two components:

$$\\sigma_i^2 = \\underbrace{\\beta_i^2 \\sigma_m^2}_{\\text{systematic}} + \\underbrace{\\sigma_{\\varepsilon_i}^2}_{\\text{idiosyncratic}}$$

### Systematic Risk (Market Risk)

$$\\sigma_{\\text{sys}}^2 = \\beta_i^2 \\cdot \\sigma_m^2$$

This risk is driven by broad market movements and **cannot be diversified away**. It is the only risk that commands a return premium under CAPM.

### Idiosyncratic Risk (Firm-Specific Risk)

$$\\sigma_{\\varepsilon}^2 = \\sigma_i^2 - \\beta_i^2 \\cdot \\sigma_m^2$$

This risk is specific to the individual asset and **can be eliminated through diversification** across many assets.

### R-Squared

The fraction of total variance explained by systematic risk is R²:

$$R^2 = \\frac{\\beta_i^2 \\sigma_m^2}{\\sigma_i^2}$$

### Your Task

Implement:
- \`systematic_risk(beta, var_market)\` — returns the systematic variance component: β² × σ_m²
- \`idiosyncratic_risk(total_var, beta, var_market)\` — returns the idiosyncratic variance component`,
  starterCode: `def systematic_risk(beta, var_market):
    pass

def idiosyncratic_risk(total_var, beta, var_market):
    pass`,
  solution: `def systematic_risk(beta, var_market):
    return beta**2 * var_market

def idiosyncratic_risk(total_var, beta, var_market):
    return total_var - beta**2 * var_market`,
  tests: [
    {
      name: "systematic risk for beta=1.2",
      code: `{{FUNC}}\nprint(round(systematic_risk(1.2, 0.04), 4))`,
      expected: "0.0576\n",
    },
    {
      name: "idiosyncratic risk for beta=1.2",
      code: `{{FUNC}}\nprint(round(idiosyncratic_risk(0.09, 1.2, 0.04), 4))`,
      expected: "0.0324\n",
    },
    {
      name: "systematic risk for beta=0.8",
      code: `{{FUNC}}\nprint(round(systematic_risk(0.8, 0.04), 4))`,
      expected: "0.0256\n",
    },
    {
      name: "idiosyncratic risk for beta=0.8",
      code: `{{FUNC}}\nprint(round(idiosyncratic_risk(0.06, 0.8, 0.04), 4))`,
      expected: "0.0344\n",
    },
  ],
};
