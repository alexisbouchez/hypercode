import type { Lesson } from "../../types";

export const kellyCriterion: Lesson = {
  id: "kelly-criterion",
  title: "Kelly Criterion Sizing",
  chapterId: "backtesting",
  content: `## Kelly Criterion

The Kelly Criterion is a formula for determining the optimal fraction of capital to wager on a bet (or trade) to maximize the long-term growth rate of wealth.

### Formula

Given:
- \`p\` = probability of winning
- \`q = 1 - p\` = probability of losing
- \`b\` = win/loss ratio (how much you win per unit risked)

\`\`\`
kelly_fraction = p - q / b
\`\`\`

### Interpretation

- **Positive Kelly**: bet this fraction of your capital
- **Zero or negative**: do not take the bet

### Half-Kelly

In practice, many traders use "half-Kelly" — betting half the Kelly fraction — to reduce volatility while sacrificing some growth:

\`\`\`
half_kelly = kelly_fraction / 2
\`\`\`

### Example

If you win 60% of the time (\`p = 0.6\`) with a 2:1 payout (\`b = 2\`):
\`\`\`
kelly = 0.6 - 0.4 / 2 = 0.6 - 0.2 = 0.4
\`\`\`

Bet 40% of your capital on each trade.

### Drawdown Limits

A **maximum drawdown limit** caps how much the portfolio can fall from its peak before the strategy stops trading. The drawdown at time t is:

\`\`\`
drawdown(t) = (peak - value(t)) / peak
\`\`\`

If drawdown exceeds the limit, the position size is set to zero. This is a critical risk control that prevents catastrophic losses — even a strategy with a positive Kelly fraction can experience devastating drawdowns.

### Volatility Targeting

**Volatility targeting** scales position size so the portfolio maintains a constant annualized volatility target. Given recent realized volatility σ_realized and a target σ_target:

\`\`\`
vol_scale = sigma_target / sigma_realized
\`\`\`

The Kelly fraction (or any position size) is then multiplied by \`vol_scale\`. When the market is calm, you trade larger; when volatile, you reduce exposure. This is the most common professional risk management technique and is used by virtually all systematic funds.

### Task

Implement:
- \`kelly_fraction(win_prob, win_loss_ratio)\` and \`half_kelly(win_prob, win_loss_ratio)\`
- \`max_drawdown(portfolio_values)\` — returns the maximum drawdown (as a positive fraction) from a list of portfolio values
- \`vol_target_scale(recent_returns, target_vol)\` — returns the scaling factor given a list of recent daily returns and an annualized target volatility. Annualize daily std by multiplying by sqrt(252). If realized vol is 0, return 1.0.`,
  starterCode: `import math

def kelly_fraction(win_prob, win_loss_ratio):
    p = win_prob
    q = 1 - p
    b = win_loss_ratio
    # TODO: return p - q / b
    pass

def half_kelly(win_prob, win_loss_ratio):
    # TODO: return half the Kelly fraction
    pass

def max_drawdown(portfolio_values):
    # TODO: compute max drawdown from peak
    pass

def vol_target_scale(recent_returns, target_vol):
    # TODO: return target_vol / annualized realized vol
    pass`,
  solution: `import math

def kelly_fraction(win_prob, win_loss_ratio):
    p = win_prob
    q = 1 - p
    b = win_loss_ratio
    return p - q / b

def half_kelly(win_prob, win_loss_ratio):
    return kelly_fraction(win_prob, win_loss_ratio) / 2

def max_drawdown(portfolio_values):
    peak = portfolio_values[0]
    max_dd = 0.0
    for v in portfolio_values:
        if v > peak:
            peak = v
        dd = (peak - v) / peak
        if dd > max_dd:
            max_dd = dd
    return max_dd

def vol_target_scale(recent_returns, target_vol):
    n = len(recent_returns)
    mean = sum(recent_returns) / n
    var = sum((r - mean) ** 2 for r in recent_returns) / n
    daily_vol = math.sqrt(var)
    realized_vol = daily_vol * math.sqrt(252)
    if realized_vol == 0:
        return 1.0
    return target_vol / realized_vol`,
  tests: [
    {
      name: "kelly_fraction(0.6, 2.0) = 0.4",
      code: `{{FUNC}}
print(round(kelly_fraction(0.6, 2.0), 4))`,
      expected: "0.4\n",
    },
    {
      name: "kelly_fraction(0.5, 1.5) = 0.1667",
      code: `{{FUNC}}
print(round(kelly_fraction(0.5, 1.5), 4))`,
      expected: "0.1667\n",
    },
    {
      name: "kelly_fraction(0.5, 1.0) = 0.0 (break-even bet)",
      code: `{{FUNC}}
print(round(kelly_fraction(0.5, 1.0), 4))`,
      expected: "0.0\n",
    },
    {
      name: "half_kelly(0.6, 2.0) = 0.2",
      code: `{{FUNC}}
print(round(half_kelly(0.6, 2.0), 4))`,
      expected: "0.2\n",
    },
    {
      name: "max_drawdown — 20% drawdown from peak",
      code: `{{FUNC}}
pv = [100, 120, 110, 96, 105, 115]
print(round(max_drawdown(pv), 4))`,
      expected: "0.2\n",
    },
    {
      name: "max_drawdown — no drawdown on monotonic increase",
      code: `{{FUNC}}
pv = [100, 110, 120, 130]
print(round(max_drawdown(pv), 4))`,
      expected: "0.0\n",
    },
    {
      name: "vol_target_scale — scales down when realized > target",
      code: `{{FUNC}}
returns = [0.01, -0.02, 0.015, -0.01, 0.005]
scale = vol_target_scale(returns, 0.10)
print(round(scale, 4))`,
      expected: "0.4831\n",
    },
    {
      name: "vol_target_scale — returns 1.0 when realized vol is zero",
      code: `{{FUNC}}
returns = [0.0, 0.0, 0.0]
print(vol_target_scale(returns, 0.10))`,
      expected: "1.0\n",
    },
  ],
};
