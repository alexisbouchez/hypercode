import type { Lesson } from "../../types";

export const securityMarketLine: Lesson = {
  id: "security-market-line",
  title: "Security Market Line",
  chapterId: "capital-market-theory",
  content: `## Security Market Line

The **Security Market Line (SML)** is the graphical representation of CAPM — it plots expected return against beta for all assets in equilibrium.

$$E[R_i] = r_f + \\beta_i \\cdot (E[R_m] - r_f)$$

### Interpreting the SML

Every fairly priced asset lies exactly on the SML:
- Assets **above** the SML are underpriced (offer excess return — positive alpha)
- Assets **below** the SML are overpriced (offer insufficient return — negative alpha)

### Excess Return (Jensen's Alpha)

The deviation of an asset's actual return from what CAPM predicts is called its **excess return** or **Jensen's alpha** (α):

$$\\alpha = R_i - E[R_i]^{\\text{CAPM}}$$

### Your Task

Implement:
- \`sml_return(rf, beta, market_premium)\` — returns the CAPM expected return: r_f + β × market_premium
- \`excess_return(actual, expected_capm)\` — returns the deviation from the SML: actual − expected`,
  starterCode: `def sml_return(rf, beta, market_premium):
    pass

def excess_return(actual, expected_capm):
    pass`,
  solution: `def sml_return(rf, beta, market_premium):
    return rf + beta * market_premium

def excess_return(actual, expected_capm):
    return actual - expected_capm`,
  tests: [
    {
      name: "SML return for beta=1.2",
      code: `{{FUNC}}\nprint(round(sml_return(0.03, 1.2, 0.08), 4))`,
      expected: "0.126\n",
    },
    {
      name: "SML return for defensive beta",
      code: `{{FUNC}}\nprint(round(sml_return(0.03, 0.7, 0.08), 4))`,
      expected: "0.086\n",
    },
    {
      name: "positive excess return (underpriced)",
      code: `{{FUNC}}\nprint(round(excess_return(0.14, 0.126), 4))`,
      expected: "0.014\n",
    },
    {
      name: "negative excess return (overpriced)",
      code: `{{FUNC}}\nprint(round(excess_return(0.10, 0.126), 4))`,
      expected: "-0.026\n",
    },
  ],
};
