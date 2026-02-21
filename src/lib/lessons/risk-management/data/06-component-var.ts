import type { Lesson } from "../../types";

export const componentVar: Lesson = {
  id: "component-var",
  title: "Component VaR",
  chapterId: "statistical-risk",
  content: `## Component VaR

**Component VaR** decomposes total portfolio VaR into the contribution from each individual position:

\`\`\`
CVaR_i = w_i × MVaR_i
\`\`\`

A key property: **component VaRs sum to total portfolio VaR**, making this a perfect decomposition tool.

### Two-Asset Formulas

Given weights \`w1, w2\`, vols \`σ1, σ2\`, correlation \`ρ\`, and multiplier \`z\`:

\`\`\`
σ_p = sqrt(w1²σ1² + w2²σ2² + 2·w1·w2·σ1·σ2·ρ)

MVaR₁ = z · (w1·σ1² + w2·σ1·σ2·ρ) / σ_p
MVaR₂ = z · (w2·σ2² + w1·σ1·σ2·ρ) / σ_p

CVaR₁ = w1 × MVaR₁
CVaR₂ = w2 × MVaR₂

CVaR₁ + CVaR₂ = z · σ_p  (total portfolio VaR)
\`\`\`

### Example

w1=0.5, w2=0.5, σ1=0.02, σ2=0.03, ρ=0.4, z=1.645  
Total VaR ≈ 0.0347  
CVaR₁ ≈ 0.0125, CVaR₂ ≈ 0.0222  
Sum = 0.0347 ✓
`,
  starterCode: `import math

def portfolio_vol(w1, w2, s1, s2, corr):
    return math.sqrt(w1**2 * s1**2 + w2**2 * s2**2 + 2 * w1 * w2 * s1 * s2 * corr)

def component_var(w1, w2, s1, s2, corr, z=1.645):
    pass
`,
  solution: `import math

def portfolio_vol(w1, w2, s1, s2, corr):
    return math.sqrt(w1**2 * s1**2 + w2**2 * s2**2 + 2 * w1 * w2 * s1 * s2 * corr)

def component_var(w1, w2, s1, s2, corr, z=1.645):
    pvol = portfolio_vol(w1, w2, s1, s2, corr)
    mv1 = z * (w1 * s1**2 + w2 * s1 * s2 * corr) / pvol
    mv2 = z * (w2 * s2**2 + w1 * s1 * s2 * corr) / pvol
    cv1 = w1 * mv1
    cv2 = w2 * mv2
    return round(cv1, 4), round(cv2, 4)
`,
  tests: [
    {
      name: "component_var w1=0.5 w2=0.5 s1=0.02 s2=0.03 corr=0.4",
      code: `{{FUNC}}\ncv1, cv2 = component_var(0.5, 0.5, 0.02, 0.03, 0.4)\nprint(cv1, cv2)`,
      expected: "0.0125 0.0222\n",
    },
    {
      name: "component_var sum equals total portfolio VaR",
      code: `{{FUNC}}\ncv1, cv2 = component_var(0.5, 0.5, 0.02, 0.03, 0.4)\nprint(round(cv1 + cv2, 4))`,
      expected: "0.0347\n",
    },
    {
      name: "component_var w1=0.6 w2=0.4 s1=0.02 s2=0.04 corr=0.3",
      code: `{{FUNC}}\ncv1, cv2 = component_var(0.6, 0.4, 0.02, 0.04, 0.3)\nprint(round(cv1 + cv2, 4))`,
      expected: "0.0373\n",
    },
    {
      name: "component_var uncorrelated — each component independent",
      code: `{{FUNC}}\ncv1, cv2 = component_var(0.5, 0.5, 0.02, 0.02, 0.0)\nprint(cv1, cv2)`,
      expected: "0.0116 0.0116\n",
    },
  ],
};
