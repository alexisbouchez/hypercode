import type { Lesson } from "../../types";

export const riskParity: Lesson = {
  id: "risk-parity",
  title: "Risk Parity Weights",
  chapterId: "factor-models",
  content: `## Risk Parity

**Risk parity** portfolios allocate capital so that each asset contributes **equally** to total portfolio risk.

For the simple case (ignoring correlations), each asset's risk contribution is proportional to w_i × σ_i. Setting these equal:

$$w_i \\cdot \\sigma_i = w_j \\cdot \\sigma_j \\quad \\forall i, j$$

### Two-Asset Risk Parity

For two assets with volatilities σ₁ and σ₂:

$$w_1 = \\frac{\\sigma_2}{\\sigma_1 + \\sigma_2}, \\quad w_2 = \\frac{\\sigma_1}{\\sigma_1 + \\sigma_2}$$

This allocates **more** weight to the **less volatile** asset.

### N-Asset Risk Parity (Inverse Volatility)

For N assets, the inverse volatility weights are:

$$w_i = \\frac{1/\\sigma_i}{\\sum_j 1/\\sigma_j}$$

### Your Task

Implement:
- \`risk_parity_2(s1, s2)\` — returns a tuple \`(w1, w2)\` of risk parity weights for two assets, each rounded to 4 decimal places
- \`risk_parity_n(sigmas)\` — returns a list of inverse volatility weights for N assets, each rounded to 4 decimal places`,
  starterCode: `def risk_parity_2(s1, s2):
    pass

def risk_parity_n(sigmas):
    pass`,
  solution: `def risk_parity_2(s1, s2):
    w1 = s2 / (s1 + s2)
    w2 = s1 / (s1 + s2)
    return round(w1, 4), round(w2, 4)

def risk_parity_n(sigmas):
    inv = [1.0 / s for s in sigmas]
    total = sum(inv)
    return [round(v / total, 4) for v in inv]`,
  tests: [
    {
      name: "2-asset risk parity (unequal vol)",
      code: `{{FUNC}}\nprint(risk_parity_2(0.1, 0.2))`,
      expected: "(0.6667, 0.3333)\n",
    },
    {
      name: "2-asset risk parity (equal vol)",
      code: `{{FUNC}}\nprint(risk_parity_2(0.15, 0.15))`,
      expected: "(0.5, 0.5)\n",
    },
    {
      name: "N-asset inverse volatility (3 assets)",
      code: `{{FUNC}}\nprint(risk_parity_n([0.1, 0.2, 0.4]))`,
      expected: "[0.5714, 0.2857, 0.1429]\n",
    },
    {
      name: "N-asset inverse volatility (2 assets)",
      code: `{{FUNC}}\nprint(risk_parity_n([0.1, 0.2]))`,
      expected: "[0.6667, 0.3333]\n",
    },
  ],
};
