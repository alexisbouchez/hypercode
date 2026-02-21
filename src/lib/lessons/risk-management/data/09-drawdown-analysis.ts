import type { Lesson } from "../../types";

export const drawdownAnalysis: Lesson = {
  id: "drawdown-analysis",
  title: "Drawdown Analysis",
  chapterId: "tail-risk",
  content: `## Drawdown Analysis

A **drawdown** measures the decline from a historical peak to the current value of an equity curve. It quantifies *ongoing* losses from the high-water mark.

### Drawdown Formula

For each time step \`t\`:
\`\`\`
peak_t = max(equity_curve[:t+1])
drawdown_t = (equity_t - peak_t) / peak_t
\`\`\`

Drawdowns are always ≤ 0 (or 0 when at a new peak).

### Maximum Drawdown

\`\`\`
max_drawdown = min(drawdowns)
\`\`\`

This is the single largest peak-to-trough decline over the entire history — a key metric for strategy risk.

### Example

Equity curve: \`[100, 110, 105, 95, 100, 108, 115]\`

| t | Value | Peak | Drawdown |
|---|-------|------|---------|
| 0 | 100 | 100 | 0.0 |
| 1 | 110 | 110 | 0.0 |
| 2 | 105 | 110 | −0.0455 |
| 3 | 95  | 110 | −0.1364 |
| 4 | 100 | 110 | −0.0909 |
| 5 | 108 | 110 | −0.0182 |
| 6 | 115 | 115 | 0.0 |

Max drawdown = **−0.1364**
`,
  starterCode: `def drawdowns(equity_curve):
    pass

def max_drawdown(equity_curve):
    pass
`,
  solution: `def drawdowns(equity_curve):
    peak = equity_curve[0]
    dds = []
    for val in equity_curve:
        if val > peak:
            peak = val
        dd = (val - peak) / peak
        dds.append(round(dd, 4))
    return dds

def max_drawdown(equity_curve):
    return min(drawdowns(equity_curve))
`,
  tests: [
    {
      name: "drawdowns on sample equity curve",
      code: `{{FUNC}}\nprint(drawdowns([100, 110, 105, 95, 100, 108, 115]))`,
      expected: "[0.0, 0.0, -0.0455, -0.1364, -0.0909, -0.0182, 0.0]\n",
    },
    {
      name: "max_drawdown on sample equity curve",
      code: `{{FUNC}}\nprint(max_drawdown([100, 110, 105, 95, 100, 108, 115]))`,
      expected: "-0.1364\n",
    },
    {
      name: "max_drawdown on deeper drawdown curve",
      code: `{{FUNC}}\nprint(max_drawdown([100, 120, 100, 80, 90, 110, 130]))`,
      expected: "-0.3333\n",
    },
    {
      name: "max_drawdown with smaller drawdown",
      code: `{{FUNC}}\nprint(max_drawdown([100, 105, 110, 108, 102, 100, 106, 112]))`,
      expected: "-0.0909\n",
    },
  ],
};
