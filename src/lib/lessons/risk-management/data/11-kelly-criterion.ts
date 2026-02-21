import type { Lesson } from "../../types";

export const kellyCriterion: Lesson = {
  id: "kelly-criterion",
  title: "Kelly Criterion",
  chapterId: "tail-risk",
  content: `## Kelly Criterion

The **Kelly Criterion** determines the optimal fraction of capital to bet on each trade to maximize long-run wealth growth. Overbetting leads to ruin; underbetting leaves growth on the table.

### Formula

\`\`\`
f* = p - q/b
\`\`\`

Where:
- \`p\` = probability of winning
- \`q = 1 - p\` = probability of losing
- \`b\` = win/loss ratio (e.g., if you gain $1.50 per $1 risked, b = 1.5)

### Adjusted Kelly with Transaction Costs

In practice, trading has costs. Subtract the cost per trade as a fraction:

\`\`\`
f_adj = f* - cost_per_trade
\`\`\`

### Example

p = 0.55, b = 1.5:
\`\`\`
f* = 0.55 - 0.45/1.5 = 0.55 - 0.30 = 0.25
\`\`\`

With cost = 0.01: f_adj = 0.24

A Kelly fraction of 0 or negative means the edge does not justify the risk.
`,
  starterCode: `def kelly_fraction(win_prob, win_loss_ratio):
    pass

def kelly_with_costs(win_prob, win_loss_ratio, cost_per_trade):
    pass
`,
  solution: `def kelly_fraction(win_prob, win_loss_ratio):
    p = win_prob
    q = 1 - p
    b = win_loss_ratio
    return p - q / b

def kelly_with_costs(win_prob, win_loss_ratio, cost_per_trade):
    return kelly_fraction(win_prob, win_loss_ratio) - cost_per_trade
`,
  tests: [
    {
      name: "kelly_fraction p=0.55 b=1.5",
      code: `{{FUNC}}\nprint(round(kelly_fraction(0.55, 1.5), 4))`,
      expected: "0.25\n",
    },
    {
      name: "kelly_fraction p=0.6 b=2.0",
      code: `{{FUNC}}\nprint(round(kelly_fraction(0.6, 2.0), 4))`,
      expected: "0.4\n",
    },
    {
      name: "kelly_with_costs p=0.55 b=1.5 cost=0.01",
      code: `{{FUNC}}\nprint(round(kelly_with_costs(0.55, 1.5, 0.01), 4))`,
      expected: "0.24\n",
    },
    {
      name: "kelly_fraction at break-even (p=0.5 b=1.0) is zero",
      code: `{{FUNC}}\nprint(round(kelly_fraction(0.5, 1.0), 4))`,
      expected: "0.0\n",
    },
  ],
};
