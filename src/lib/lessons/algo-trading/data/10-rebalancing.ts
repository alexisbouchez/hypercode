import type { Lesson } from "../../types";

export const rebalancing: Lesson = {
  id: "rebalancing",
  title: "Portfolio Rebalancing",
  chapterId: "mean-reversion",
  content: `## Portfolio Rebalancing

Portfolio rebalancing is the process of realigning the weights of a portfolio to match a target allocation. Over time, winning assets grow and losing assets shrink, causing the portfolio to drift from the desired allocation.

### Rebalance Trades

To rebalance, we compute how many shares (or units) of each asset to buy or sell:

\`\`\`
trade[i] = (target_weights[i] - current_weights[i]) * portfolio_value / prices[i]
\`\`\`

- A **positive** trade means buying more of that asset.
- A **negative** trade means selling some of that asset.
- A **zero** trade means no change needed.

### Task

Implement \`rebalance(current_weights, target_weights, prices, portfolio_value)\` that returns a list of trade sizes for each asset.`,
  starterCode: `def rebalance(current_weights, target_weights, prices, portfolio_value):
    trades = []
    # TODO: compute (target - current) * portfolio_value / price for each asset
    return trades`,
  solution: `def rebalance(current_weights, target_weights, prices, portfolio_value):
    return [
        (t - c) * portfolio_value / p
        for c, t, p in zip(current_weights, target_weights, prices)
    ]`,
  tests: [
    {
      name: "rebalance — buy asset when underweight",
      code: `{{FUNC}}
trades = rebalance([0.4, 0.4, 0.2], [0.5, 0.3, 0.2], [100.0, 50.0, 200.0], 10000.0)
print(round(trades[0], 4))`,
      expected: "10.0\n",
    },
    {
      name: "rebalance — sell asset when overweight",
      code: `{{FUNC}}
trades = rebalance([0.4, 0.4, 0.2], [0.5, 0.3, 0.2], [100.0, 50.0, 200.0], 10000.0)
print(round(trades[1], 4))`,
      expected: "-20.0\n",
    },
    {
      name: "rebalance — zero trade when weight unchanged",
      code: `{{FUNC}}
trades = rebalance([0.4, 0.4, 0.2], [0.5, 0.3, 0.2], [100.0, 50.0, 200.0], 10000.0)
print(round(trades[2], 4))`,
      expected: "0.0\n",
    },
    {
      name: "rebalance — correct trades for different values",
      code: `{{FUNC}}
trades = rebalance([0.3, 0.3, 0.4], [0.4, 0.4, 0.2], [50.0, 100.0, 25.0], 5000.0)
print([round(x, 4) for x in trades])`,
      expected: "[10.0, 5.0, -40.0]\n",
    },
  ],
};
