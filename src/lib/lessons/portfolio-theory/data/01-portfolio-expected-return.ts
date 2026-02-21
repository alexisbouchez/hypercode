import type { Lesson } from "../../types";

export const portfolioExpectedReturn: Lesson = {
  id: "portfolio-expected-return",
  title: "Portfolio Expected Return",
  chapterId: "return-risk",
  content: `## Portfolio Expected Return

The **expected return of a portfolio** is the weighted average of the expected returns of its constituent assets.

If a portfolio holds assets with returns r₁, r₂, …, rₙ and corresponding weights w₁, w₂, …, wₙ (where the weights sum to 1), the portfolio return is:

$$E[R_p] = \\sum_{i=1}^{n} w_i \\cdot r_i$$

This is simply the dot product of the weight vector and the return vector.

### Example

A portfolio holds two assets:
- Asset A: weight 0.6, expected return 10%
- Asset B: weight 0.4, expected return 15%

Portfolio return = 0.6 × 0.10 + 0.4 × 0.15 = **0.12** (12%)

### Your Task

Implement \`portfolio_return(weights, returns)\` that computes the expected return of a portfolio given a list of weights and a list of expected returns.`,
  starterCode: `def portfolio_return(weights, returns):
    pass`,
  solution: `def portfolio_return(weights, returns):
    return sum(w * r for w, r in zip(weights, returns))`,
  tests: [
    {
      name: "equal weights, two assets",
      code: `{{FUNC}}\nprint(round(portfolio_return([0.5, 0.5], [0.1, 0.2]), 4))`,
      expected: "0.15\n",
    },
    {
      name: "three assets",
      code: `{{FUNC}}\nprint(round(portfolio_return([0.4, 0.3, 0.3], [0.1, 0.08, 0.12]), 4))`,
      expected: "0.1\n",
    },
    {
      name: "single asset",
      code: `{{FUNC}}\nprint(round(portfolio_return([1.0], [0.07]), 4))`,
      expected: "0.07\n",
    },
    {
      name: "60/40 portfolio",
      code: `{{FUNC}}\nprint(round(portfolio_return([0.6, 0.4], [0.05, 0.15]), 4))`,
      expected: "0.09\n",
    },
  ],
};
