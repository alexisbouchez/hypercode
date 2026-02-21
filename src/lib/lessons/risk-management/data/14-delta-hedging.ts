import type { Lesson } from "../../types";

export const deltaHedging: Lesson = {
  id: "delta-hedging",
  title: "Greeks-Based Risk (Delta Hedging)",
  chapterId: "position-management",
  content: `## Greeks-Based Risk: Delta Hedging

**Delta (Δ)** measures how much an option's price changes per $1 move in the underlying asset. Delta hedging neutralizes this exposure by holding an offsetting stock position.

### Hedge Quantity

To delta-hedge a position of \`option_quantity\` options each with delta \`Δ\`:

\`\`\`
hedge_quantity = -delta × option_quantity
\`\`\`

- Long call (Δ > 0) → short shares to hedge
- Long put (Δ < 0) → long shares to hedge

### Hedged P&L

When the underlying moves from \`S0\` to \`S1\`:

\`\`\`
pnl = option_pnl + hedge_quantity × (S1 - S0)
\`\`\`

In a perfect delta hedge, the two components offset each other, leaving a small residual from gamma (second-order effects).

### Example

Long 100 call options with Δ = 0.6:  
hedge_quantity = −0.6 × 100 = **−60 shares** (short 60 shares)

If S moves from $100 to $105 and options lose $3:  
pnl = −3 + (−60) × (105 − 100) = −3 − 300 = **−$303**

This is the gamma P&L — the hedge is not perfect over large moves.
`,
  starterCode: `def hedge_quantity(delta, option_quantity):
    pass

def pnl_delta_hedge(S0, S1, delta, option_pnl, hedge_qty):
    pass
`,
  solution: `def hedge_quantity(delta, option_quantity):
    return -delta * option_quantity

def pnl_delta_hedge(S0, S1, delta, option_pnl, hedge_qty):
    return option_pnl + hedge_qty * (S1 - S0)
`,
  tests: [
    {
      name: "hedge_quantity long call delta=0.6 quantity=100",
      code: `{{FUNC}}\nprint(round(hedge_quantity(0.6, 100), 4))`,
      expected: "-60.0\n",
    },
    {
      name: "hedge_quantity long put delta=-0.4 quantity=200",
      code: `{{FUNC}}\nprint(round(hedge_quantity(-0.4, 200), 4))`,
      expected: "80.0\n",
    },
    {
      name: "pnl_delta_hedge call option S rises",
      code: `{{FUNC}}\nprint(round(pnl_delta_hedge(100, 105, 0.6, -3.0, -60), 4))`,
      expected: "-303.0\n",
    },
    {
      name: "pnl_delta_hedge put option S falls",
      code: `{{FUNC}}\nprint(round(pnl_delta_hedge(100, 95, -0.4, 4.0, 80), 4))`,
      expected: "-396.0\n",
    },
  ],
};
