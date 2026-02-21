import type { Lesson } from "../../types";

export const marginalVar: Lesson = {
  id: "marginal-var",
  title: "Marginal VaR",
  chapterId: "statistical-risk",
  content: `## Marginal VaR

**Marginal VaR** measures how portfolio VaR changes when you increase a position by a small amount. It is the partial derivative of portfolio VaR with respect to a position weight.

### Two-Asset Portfolio

For a portfolio with weights \`w1, w2\` and asset volatilities \`σ1, σ2\` and correlation \`ρ\`:

**Portfolio volatility:**
\`\`\`
σ_p = sqrt(w1²σ1² + w2²σ2² + 2·w1·w2·σ1·σ2·ρ)
\`\`\`

**Marginal VaR w.r.t. w1:**
\`\`\`
MVaR₁ = z · (w1·σ1² + w2·σ1·σ2·ρ) / σ_p
\`\`\`

where \`z\` is the confidence-level multiplier (default 1.645 for 95%).

### Interpretation

A marginal VaR of 0.025 means that increasing the allocation to asset 1 by 1 percentage point adds approximately 0.025 × 0.01 = 0.025 basis points of VaR.

### Example

w1=0.5, w2=0.5, σ1=0.02, σ2=0.03, ρ=0.4, z=1.645  
σ_p = sqrt(0.25×0.0004 + 0.25×0.0009 + 2×0.25×0.0006×0.4) = sqrt(0.000445)  
MVaR₁ = 1.645 × (0.5×0.0004 + 0.5×0.02×0.03×0.4) / σ_p ≈ **0.025**
`,
  starterCode: `import math

def portfolio_vol(w1, w2, s1, s2, corr):
    pass

def marginal_var_2(w1, w2, s1, s2, corr, z=1.645):
    pass
`,
  solution: `import math

def portfolio_vol(w1, w2, s1, s2, corr):
    return math.sqrt(w1**2 * s1**2 + w2**2 * s2**2 + 2 * w1 * w2 * s1 * s2 * corr)

def marginal_var_2(w1, w2, s1, s2, corr, z=1.645):
    pvol = portfolio_vol(w1, w2, s1, s2, corr)
    return z * (w1 * s1**2 + w2 * s1 * s2 * corr) / pvol
`,
  tests: [
    {
      name: "marginal_var_2 w1=0.5 w2=0.5 s1=0.02 s2=0.03 corr=0.4",
      code: `{{FUNC}}\nprint(round(marginal_var_2(0.5, 0.5, 0.02, 0.03, 0.4), 4))`,
      expected: "0.025\n",
    },
    {
      name: "marginal_var_2 w1=0.6 w2=0.4 s1=0.02 s2=0.04 corr=0.3",
      code: `{{FUNC}}\nprint(round(marginal_var_2(0.6, 0.4, 0.02, 0.04, 0.3), 4))`,
      expected: "0.0244\n",
    },
    {
      name: "marginal_var_2 uncorrelated equal weights",
      code: `{{FUNC}}\nprint(round(marginal_var_2(0.5, 0.5, 0.02, 0.02, 0.0), 4))`,
      expected: "0.0233\n",
    },
    {
      name: "portfolio_vol equal weights equal vol no corr",
      code: `{{FUNC}}\nprint(round(portfolio_vol(0.5, 0.5, 0.02, 0.02, 0.0), 4))`,
      expected: "0.0141\n",
    },
  ],
};
