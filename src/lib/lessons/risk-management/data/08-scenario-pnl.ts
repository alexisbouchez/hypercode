import type { Lesson } from "../../types";

export const scenarioPnl: Lesson = {
  id: "scenario-pnl",
  title: "Scenario P&L",
  chapterId: "statistical-risk",
  content: `## Scenario P&L

**Scenario P&L** measures how a portfolio's dollar profit or loss changes under a hypothetical market move. Unlike stress testing with absolute position values, here we use portfolio weights and apply uniform shocks relative to total portfolio value.

### Formula

\`\`\`
P&L = portfolio_value × Σ(w_i × shock_i)
\`\`\`

Where \`w_i\` are the allocation weights (summing to 1) and \`shock_i\` are percentage changes for each asset.

### Interpretation

- Positive P&L → the scenario is favorable
- Negative P&L → the scenario causes a loss
- Magnitude shows dollar risk from the scenario

### Example

portfolio_value = $1,000,000  
weights = [0.5, 0.5]  
shocks = [−10%, +5%]  
P&L = 1,000,000 × (0.5×(−0.10) + 0.5×0.05)  
    = 1,000,000 × (−0.05 + 0.025)  
    = 1,000,000 × (−0.025)  
    = **−$25,000**
`,
  starterCode: `def scenario_pnl(portfolio_value, weights, shocks):
    pass
`,
  solution: `def scenario_pnl(portfolio_value, weights, shocks):
    pnl = portfolio_value * sum(w * s for w, s in zip(weights, shocks))
    return round(pnl, 4)
`,
  tests: [
    {
      name: "scenario_pnl equal weights mixed shocks",
      code: `{{FUNC}}\nprint(scenario_pnl(1000000, [0.5, 0.5], [-0.1, 0.05]))`,
      expected: "-25000.0\n",
    },
    {
      name: "scenario_pnl unequal weights both negative",
      code: `{{FUNC}}\nprint(scenario_pnl(500000, [0.6, 0.4], [-0.05, 0.03]))`,
      expected: "-9000.0\n",
    },
    {
      name: "scenario_pnl three assets",
      code: `{{FUNC}}\nprint(scenario_pnl(1000000, [0.3, 0.4, 0.3], [-0.1, -0.05, 0.1]))`,
      expected: "-20000.0\n",
    },
    {
      name: "scenario_pnl all assets up",
      code: `{{FUNC}}\nprint(scenario_pnl(200000, [0.4, 0.6], [0.10, 0.05]))`,
      expected: "14000.0\n",
    },
  ],
};
