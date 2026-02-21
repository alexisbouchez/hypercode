import type { Lesson } from "../../types";

export const stressTesting: Lesson = {
  id: "stress-testing",
  title: "Stress Testing Scenarios",
  chapterId: "statistical-risk",
  content: `## Stress Testing Scenarios

**Stress testing** evaluates portfolio performance under extreme but plausible market conditions — things like a 2008-style crash, a sudden rate spike, or a flash crash.

### Apply Stress

Given a portfolio and a scenario, compute the new portfolio value:

\`\`\`
P&L = Σ (weight_i × asset_value_i × shock_i)
new_value = portfolio_value + P&L
\`\`\`

Each position is defined by its weight and current market value. Each shock is a percentage change (e.g. -0.10 = −10%).

### Common Stress Scenarios

| Scenario | Equity | Credit | Rates |
|----------|--------|--------|-------|
| 2008 Crisis | −40% | −30% | −10% |
| Tech Crash | −35% | 0% | +5% |
| Rate Spike | −10% | −15% | +30% |

### Example

Portfolio value = $1,000,000  
Positions: [(0.6, $600,000), (0.4, $400,000)]  
Shocks: [−10%, +5%]  
P&L = 0.6×600000×(−0.10) + 0.4×400000×(0.05) = −36000 + 8000 = −28000  
New value = **$972,000**
`,
  starterCode: `def apply_stress(portfolio_value, positions, scenario_shocks):
    pass
`,
  solution: `def apply_stress(portfolio_value, positions, scenario_shocks):
    pnl = sum(w * av * s for (w, av), s in zip(positions, scenario_shocks))
    return round(portfolio_value + pnl, 4)
`,
  tests: [
    {
      name: "apply_stress single asset -10% shock",
      code: `{{FUNC}}\nprint(apply_stress(1000000, [(1.0, 1000000)], [-0.10]))`,
      expected: "900000.0\n",
    },
    {
      name: "apply_stress two assets mixed shocks",
      code: `{{FUNC}}\nprint(apply_stress(1000000, [(0.6, 600000), (0.4, 400000)], [-0.10, 0.05]))`,
      expected: "972000.0\n",
    },
    {
      name: "apply_stress three assets",
      code: `{{FUNC}}\nprint(apply_stress(500000, [(0.4, 200000), (0.3, 150000), (0.3, 150000)], [-0.05, -0.10, 0.02]))`,
      expected: "492400.0\n",
    },
    {
      name: "apply_stress no shock — portfolio unchanged",
      code: `{{FUNC}}\nprint(apply_stress(750000, [(0.5, 375000), (0.5, 375000)], [0.0, 0.0]))`,
      expected: "750000.0\n",
    },
  ],
};
