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

### Task

Implement \`kelly_fraction(win_prob, win_loss_ratio)\` and \`half_kelly(win_prob, win_loss_ratio)\`.`,
  starterCode: `def kelly_fraction(win_prob, win_loss_ratio):
    p = win_prob
    q = 1 - p
    b = win_loss_ratio
    # TODO: return p - q / b
    pass

def half_kelly(win_prob, win_loss_ratio):
    # TODO: return half the Kelly fraction
    pass`,
  solution: `def kelly_fraction(win_prob, win_loss_ratio):
    p = win_prob
    q = 1 - p
    b = win_loss_ratio
    return p - q / b

def half_kelly(win_prob, win_loss_ratio):
    return kelly_fraction(win_prob, win_loss_ratio) / 2`,
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
  ],
};
