import type { Lesson } from "../../types";

export const jensensAlpha: Lesson = {
  id: "jensens-alpha",
  title: "Jensen's Alpha",
  chapterId: "capital-market-theory",
  content: `## Jensen's Alpha

**Jensen's alpha** (α) measures a portfolio's (or asset's) risk-adjusted outperformance relative to what CAPM predicts. It was introduced by Michael Jensen in 1968 to evaluate mutual fund managers.

$$\\alpha_i = R_i - [r_f + \\beta_i (R_m - r_f)]$$

where:
- R_i = actual return of the asset/portfolio
- r_f = risk-free rate
- β_i = asset's beta
- R_m = actual market return

### Interpretation

- **α > 0**: the manager (or asset) generated returns above what CAPM predicts — positive skill or mispricing
- **α = 0**: performance perfectly in line with systematic risk
- **α < 0**: underperformance relative to CAPM prediction

### Note

Jensen's alpha uses **realized** market and asset returns (not just expected), making it useful for performance attribution after the fact.

### Your Task

Implement \`jensens_alpha(actual_return, rf, beta, market_return)\` that computes Jensen's alpha.`,
  starterCode: `def jensens_alpha(actual_return, rf, beta, market_return):
    pass`,
  solution: `def jensens_alpha(actual_return, rf, beta, market_return):
    return actual_return - (rf + beta * (market_return - rf))`,
  tests: [
    {
      name: "positive alpha (outperformance)",
      code: `{{FUNC}}\nprint(round(jensens_alpha(0.14, 0.03, 1.2, 0.10), 4))`,
      expected: "0.026\n",
    },
    {
      name: "negative alpha (underperformance)",
      code: `{{FUNC}}\nprint(round(jensens_alpha(0.08, 0.03, 0.8, 0.10), 4))`,
      expected: "-0.006\n",
    },
    {
      name: "zero alpha (on SML)",
      code: `{{FUNC}}\nprint(round(jensens_alpha(0.10, 0.03, 1.0, 0.10), 4))`,
      expected: "0.0\n",
    },
    {
      name: "small positive alpha",
      code: `{{FUNC}}\nprint(round(jensens_alpha(0.12, 0.03, 1.2, 0.10), 4))`,
      expected: "0.006\n",
    },
  ],
};
