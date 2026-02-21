import type { Lesson } from "../../types";

export const sharpeSortino: Lesson = {
  id: "sharpe-sortino",
  title: "Sharpe & Sortino Ratios",
  chapterId: "risk-metrics",
  content: `## Sharpe & Sortino Ratios

Risk-adjusted return metrics let you compare strategies that have different risk profiles.

**Sharpe Ratio** — excess return per unit of total risk (annualized):
$$\\text{Sharpe} = \\frac{\\bar{r} - r_f}{\\sigma} \\times \\sqrt{252}$$

where $\\bar{r}$ is the mean daily return, $r_f$ is the daily risk-free rate, and $\\sigma$ is the sample std of daily returns.

**Sortino Ratio** — penalizes only downside volatility:
$$\\text{Sortino} = \\frac{\\bar{r} - r_f}{\\sigma_{down}} \\times \\sqrt{252}$$

where $\\sigma_{down}$ is the sample std computed only over returns below $r_f$ (the "downside deviation").

The Sortino ratio is preferred when return distributions are asymmetric, because upside volatility is not a risk — it's a reward.

### Your Task

Implement:
- \`sharpe_ratio(returns, rf=0.0)\` — annualized Sharpe ratio
- \`sortino_ratio(returns, rf=0.0)\` — annualized Sortino ratio using downside std`,
  starterCode: `import math

def sharpe_ratio(returns, rf=0.0):
    # (mean - rf) / std * sqrt(252), using sample std
    pass

def sortino_ratio(returns, rf=0.0):
    # (mean - rf) / downside_std * sqrt(252)
    # downside_std = sample std of returns below rf
    pass`,
  solution: `import math

def sharpe_ratio(returns, rf=0.0):
    n = len(returns)
    m = sum(returns) / n
    std = math.sqrt(sum((r - m) ** 2 for r in returns) / (n - 1))
    return (m - rf) / std * math.sqrt(252)

def sortino_ratio(returns, rf=0.0):
    n = len(returns)
    m = sum(returns) / n
    down_rets = [r for r in returns if r < rf]
    nd = len(down_rets)
    md = sum(down_rets) / nd
    down_std = math.sqrt(sum((r - md) ** 2 for r in down_rets) / (nd - 1))
    return (m - rf) / down_std * math.sqrt(252)`,
  tests: [
    {
      name: "sharpe_ratio of sample returns equals ~9.0171",
      code: `{{FUNC}}
returns = [0.01, -0.005, 0.008, 0.012, -0.003, 0.007, 0.002, -0.001, 0.015, -0.004]
print(round(sharpe_ratio(returns), 4))`,
      expected: "9.0171\n",
    },
    {
      name: "sortino_ratio of sample returns equals ~38.1102",
      code: `{{FUNC}}
returns = [0.01, -0.005, 0.008, 0.012, -0.003, 0.007, 0.002, -0.001, 0.015, -0.004]
print(round(sortino_ratio(returns), 4))`,
      expected: "38.1102\n",
    },
    {
      name: "sharpe_ratio with all positive returns is positive",
      code: `{{FUNC}}
returns = [0.01, 0.02, 0.015, 0.01, 0.012]
result = sharpe_ratio(returns)
print(result > 0)`,
      expected: "True\n",
    },
  ],
};
