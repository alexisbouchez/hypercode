import type { Lesson } from "../../types";

export const fixedFractional: Lesson = {
  id: "fixed-fractional",
  title: "Fixed Fractional Position Sizing",
  chapterId: "tail-risk",
  content: `## Fixed Fractional Position Sizing

**Fixed fractional** position sizing is the most widely used risk-based position sizing method. You risk a fixed percentage of your capital on every trade, with the position size adjusted to keep the dollar risk constant.

### Formula

\`\`\`
position_size = capital × risk_per_trade / stop_loss_pct
\`\`\`

Where:
- \`capital\` — total trading capital
- \`risk_per_trade\` — fraction of capital to risk (e.g., 0.02 = 2%)
- \`stop_loss_pct\` — distance from entry to stop loss as a fraction (e.g., 0.05 = 5%)

### Intuition

If you're willing to lose 2% of $100,000 = $2,000, and your stop is 5% below entry, you can buy a position worth $2,000 / 0.05 = $40,000.

### Example

- Capital = $100,000
- Risk per trade = 2%
- Stop loss = 5%
- Position size = 100,000 × 0.02 / 0.05 = **$40,000**

Tighter stops → larger positions (same dollar risk).  
Wider stops → smaller positions.
`,
  starterCode: `def position_size_ff(capital, risk_per_trade, stop_loss_pct):
    pass
`,
  solution: `def position_size_ff(capital, risk_per_trade, stop_loss_pct):
    return capital * risk_per_trade / stop_loss_pct
`,
  tests: [
    {
      name: "position_size_ff capital=100000 risk=2% stop=5%",
      code: `{{FUNC}}\nprint(round(position_size_ff(100000, 0.02, 0.05), 4))`,
      expected: "40000.0\n",
    },
    {
      name: "position_size_ff capital=500000 risk=1% stop=2%",
      code: `{{FUNC}}\nprint(round(position_size_ff(500000, 0.01, 0.02), 4))`,
      expected: "250000.0\n",
    },
    {
      name: "position_size_ff capital=250000 risk=2% stop=10%",
      code: `{{FUNC}}\nprint(round(position_size_ff(250000, 0.02, 0.10), 4))`,
      expected: "50000.0\n",
    },
    {
      name: "position_size_ff capital=100000 risk=1% stop=2.5%",
      code: `{{FUNC}}\nprint(round(position_size_ff(100000, 0.01, 0.025), 4))`,
      expected: "40000.0\n",
    },
  ],
};
