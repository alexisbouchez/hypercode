import type { Lesson } from "../../types";

export const riskAdjustedMetrics: Lesson = {
  id: "risk-adjusted-metrics",
  title: "Risk-Adjusted Performance Metrics",
  chapterId: "position-management",
  content: `## Risk-Adjusted Performance Metrics

Raw returns don't tell the full story. **Risk-adjusted metrics** reveal whether a strategy compensates you adequately for the risk taken.

### Information Ratio (IR)

The IR measures a portfolio's active return per unit of active risk vs a benchmark:

\`\`\`
IR = mean(active_returns) / std(active_returns) × sqrt(252)
\`\`\`

Where \`active_returns[i] = portfolio_return[i] - benchmark_return[i]\`.

The \`sqrt(252)\` annualizes a daily IR (252 trading days per year).

### Calmar Ratio

Measures annualized return divided by maximum drawdown:

\`\`\`
Calmar = annual_return / |max_drawdown_pct|
\`\`\`

A Calmar of 2.0 means you earn twice your worst drawdown each year.

### Sample Standard Deviation

Use \`n - 1\` in the denominator (Bessel's correction):

\`\`\`
std = sqrt(Σ(x - mean)² / (n - 1))
\`\`\`

### Example

Calmar ratio = 0.15 / 0.25 = **0.6**  
(15% annual return with 25% max drawdown)
`,
  starterCode: `import math

def information_ratio(active_returns, benchmark_returns):
    pass

def calmar_ratio(annual_return, max_drawdown_pct):
    pass
`,
  solution: `import math

def information_ratio(active_returns, benchmark_returns):
    active = [a - b for a, b in zip(active_returns, benchmark_returns)]
    n = len(active)
    mean_a = sum(active) / n
    std_a = math.sqrt(sum((x - mean_a) ** 2 for x in active) / (n - 1))
    return mean_a / std_a * math.sqrt(252)

def calmar_ratio(annual_return, max_drawdown_pct):
    return annual_return / abs(max_drawdown_pct)
`,
  tests: [
    {
      name: "information_ratio with positive active return",
      code: `{{FUNC}}\nactive = [0.01, 0.02, -0.01, 0.03, 0.01]\nbench = [0.008, 0.015, -0.005, 0.025, 0.007]\nprint(round(information_ratio(active, bench), 4))`,
      expected: "7.7003\n",
    },
    {
      name: "calmar_ratio annual=15% max_dd=25%",
      code: `{{FUNC}}\nprint(round(calmar_ratio(0.15, -0.25), 4))`,
      expected: "0.6\n",
    },
    {
      name: "calmar_ratio annual=20% max_dd=10%",
      code: `{{FUNC}}\nprint(round(calmar_ratio(0.20, -0.10), 4))`,
      expected: "2.0\n",
    },
    {
      name: "information_ratio higher active return",
      code: `{{FUNC}}\nactive = [0.02, 0.015, 0.025, 0.018, 0.022]\nbench = [0.01, 0.01, 0.01, 0.01, 0.01]\nprint(round(information_ratio(active, bench), 4))`,
      expected: "41.6885\n",
    },
  ],
};
