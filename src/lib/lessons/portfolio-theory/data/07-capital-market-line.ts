import type { Lesson } from "../../types";

export const capitalMarketLine: Lesson = {
  id: "capital-market-line",
  title: "Capital Market Line",
  chapterId: "optimization",
  content: `## Capital Market Line

The **Capital Market Line (CML)** describes the risk-return trade-off for efficient portfolios formed by combining the **tangency portfolio** T with the **risk-free asset**.

Any point on the CML is a blend of the risk-free asset (σ = 0, return = r_f) and the tangency portfolio (σ = σ_T, return = μ_T).

The equation of the CML is:

$$\\mu(\\sigma) = r_f + \\frac{\\mu_T - r_f}{\\sigma_T} \\cdot \\sigma$$

The slope (μ_T − r_f) / σ_T is the **Sharpe ratio of the tangency portfolio** — also called the **price of risk** or **market price of risk**.

### Interpretation

- At σ = 0: you hold only the risk-free asset, earning r_f
- At σ = σ_T: you hold only the tangency portfolio, earning μ_T
- At σ > σ_T: you borrow at r_f to leverage into the tangency portfolio

### Your Task

Implement \`cml_return(mu_t, s_t, rf, sigma)\` that returns the expected return on the CML at a given level of risk σ.`,
  starterCode: `def cml_return(mu_t, s_t, rf, sigma):
    pass`,
  solution: `def cml_return(mu_t, s_t, rf, sigma):
    return rf + (mu_t - rf) / s_t * sigma`,
  tests: [
    {
      name: "CML return at sigma=0.1",
      code: `{{FUNC}}\nprint(round(cml_return(0.12, 0.15, 0.03, 0.1), 4))`,
      expected: "0.09\n",
    },
    {
      name: "CML return at sigma=0 (risk-free rate)",
      code: `{{FUNC}}\nprint(round(cml_return(0.12, 0.15, 0.03, 0.0), 4))`,
      expected: "0.03\n",
    },
    {
      name: "CML return at sigma=sigma_T (tangency portfolio)",
      code: `{{FUNC}}\nprint(round(cml_return(0.12, 0.15, 0.03, 0.15), 4))`,
      expected: "0.12\n",
    },
    {
      name: "CML return with different parameters",
      code: `{{FUNC}}\nprint(round(cml_return(0.10, 0.20, 0.02, 0.10), 4))`,
      expected: "0.06\n",
    },
  ],
};
