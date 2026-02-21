import type { Lesson } from "../../types";

export const slippageCosts: Lesson = {
  id: "slippage-costs",
  title: "Slippage & Transaction Costs",
  chapterId: "mean-reversion",
  content: `## Slippage & Transaction Costs

Real trading incurs costs that reduce profitability. Two major sources are:

### Slippage

Slippage is the difference between the expected price and the actual execution price. When buying, you pay slightly more; when selling, you receive slightly less.

\`\`\`
effective_price = price * (1 + direction * slippage)
\`\`\`

- \`direction = 1\` for a buy (price goes up slightly)
- \`direction = -1\` for a sell (price goes down slightly)

### Net Return

After accounting for trading costs (spread, commission, etc.), the net return is simply:

\`\`\`
net_return = gross_return - trade_cost_pct
\`\`\`

### Task

Implement:
- \`apply_slippage(price, direction, slippage=0.001)\` — returns the effective execution price
- \`net_return(gross_return, trade_cost_pct)\` — returns the return after costs`,
  starterCode: `def apply_slippage(price, direction, slippage=0.001):
    # TODO: return price * (1 + direction * slippage)
    pass

def net_return(gross_return, trade_cost_pct):
    # TODO: return gross_return minus cost
    pass`,
  solution: `def apply_slippage(price, direction, slippage=0.001):
    return price * (1 + direction * slippage)

def net_return(gross_return, trade_cost_pct):
    return gross_return - trade_cost_pct`,
  tests: [
    {
      name: "apply_slippage — buy price is higher",
      code: `{{FUNC}}
print(round(apply_slippage(100.0, 1, 0.001), 4))`,
      expected: "100.1\n",
    },
    {
      name: "apply_slippage — sell price is lower",
      code: `{{FUNC}}
print(round(apply_slippage(100.0, -1, 0.001), 4))`,
      expected: "99.9\n",
    },
    {
      name: "apply_slippage — larger slippage",
      code: `{{FUNC}}
print(round(apply_slippage(200.0, 1, 0.002), 4))`,
      expected: "200.4\n",
    },
    {
      name: "net_return — gross return minus cost",
      code: `{{FUNC}}
print(round(net_return(0.05, 0.001), 4))`,
      expected: "0.049\n",
    },
  ],
};
