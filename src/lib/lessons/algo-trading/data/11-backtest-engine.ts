import type { Lesson } from "../../types";

export const backtestEngine: Lesson = {
  id: "backtest-engine",
  title: "Backtest Engine (P&L Tracking)",
  chapterId: "mean-reversion",
  content: `## Backtest Engine

A backtest engine simulates a trading strategy on historical data to estimate its profitability. We track portfolio value over time.

### Rules

- **Signals**: \`1\` = long (buy), \`-1\` = short (sell), \`0\` = flat (no position)
- Positions are entered at the current bar's price when the signal changes.
- The portfolio value at the next bar is: \`cash + position * prices[t+1]\`

### Algorithm

Starting with \`initial_capital\`:

\`\`\`
portfolio = [initial_capital]
position = 0
cash = initial_capital

for t in range(len(signals) - 1):
    sig = signals[t]
    if sig != position:
        cash += position * prices[t]   # close existing position
        if sig != 0:
            cash -= sig * prices[t]    # open new position
        position = sig
    portfolio.append(cash + position * prices[t+1])
\`\`\`

### Task

Implement \`backtest(prices, signals, initial_capital=10000.0)\` that returns a list of portfolio values over time (same length as \`prices\`).`,
  starterCode: `def backtest(prices, signals, initial_capital=10000.0):
    portfolio = [initial_capital]
    position = 0
    cash = initial_capital
    for t in range(len(signals) - 1):
        sig = signals[t]
        if sig != position:
            # TODO: close current position, open new position
            pass
        portfolio_value = cash + position * prices[t + 1]
        portfolio.append(portfolio_value)
    return portfolio`,
  solution: `def backtest(prices, signals, initial_capital=10000.0):
    portfolio = [initial_capital]
    position = 0
    cash = initial_capital
    for t in range(len(signals) - 1):
        sig = signals[t]
        if sig != position:
            cash += position * prices[t]
            if sig != 0:
                cash -= sig * prices[t]
            position = sig
        portfolio_value = cash + position * prices[t + 1]
        portfolio.append(portfolio_value)
    return portfolio`,
  tests: [
    {
      name: "backtest — initial portfolio value equals initial capital",
      code: `{{FUNC}}
result = backtest([100, 102, 104, 103, 105], [1, 1, 1, -1, 0], 10000.0)
print(result[0])`,
      expected: "10000.0\n",
    },
    {
      name: "backtest — long position gains on up move",
      code: `{{FUNC}}
result = backtest([100, 102, 104, 103, 105], [1, 1, 1, -1, 0], 10000.0)
print(result[1])`,
      expected: "10002.0\n",
    },
    {
      name: "backtest — flat position preserves capital",
      code: `{{FUNC}}
result = backtest([100, 102, 104, 103, 105], [0, 0, 0, 0, 0], 10000.0)
print(result)`,
      expected: "[10000.0, 10000.0, 10000.0, 10000.0, 10000.0]\n",
    },
    {
      name: "backtest — full portfolio values for long/short strategy",
      code: `{{FUNC}}
result = backtest([100, 102, 104, 103, 105], [1, 1, 1, -1, 0], 10000.0)
print([round(x, 4) for x in result])`,
      expected: "[10000.0, 10002.0, 10004.0, 10003.0, 10001.0]\n",
    },
  ],
};
