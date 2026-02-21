import type { Lesson } from "../../types";

export const backtestSharpe: Lesson = {
  id: "backtest-sharpe",
  title: "Sharpe Ratio via Backtest",
  chapterId: "backtesting",
  content: `## Sharpe Ratio

The Sharpe ratio measures risk-adjusted return. A higher Sharpe ratio means more return per unit of risk. It is one of the most widely used metrics for evaluating trading strategies.

### Formula

Given a series of daily portfolio values:

1. Compute daily returns: \`r[t] = portfolio[t] / portfolio[t-1] - 1\`
2. Compute mean excess return: \`mean_excess = mean(r) - rf / 252\`
   (where \`rf\` is the annualized risk-free rate, divided by 252 trading days)
3. Compute population standard deviation of returns
4. Annualize: multiply by \`sqrt(252)\`

\`\`\`
Sharpe = (mean_excess / std) * sqrt(252)
\`\`\`

If \`std == 0\`, return \`0.0\`.

### Task

Implement \`backtest_sharpe(portfolio_values, rf=0.0)\` that returns the annualized Sharpe ratio of the portfolio.`,
  starterCode: `import math

def backtest_sharpe(portfolio_values, rf=0.0):
    returns = [(portfolio_values[i] / portfolio_values[i - 1]) - 1
               for i in range(1, len(portfolio_values))]
    n = len(returns)
    mean = sum(returns) / n
    excess = mean - rf / 252
    variance = sum((r - mean) ** 2 for r in returns) / n
    std = math.sqrt(variance)
    # TODO: return annualized Sharpe (handle std == 0)
    pass`,
  solution: `import math

def backtest_sharpe(portfolio_values, rf=0.0):
    returns = [(portfolio_values[i] / portfolio_values[i - 1]) - 1
               for i in range(1, len(portfolio_values))]
    n = len(returns)
    mean = sum(returns) / n
    excess = mean - rf / 252
    variance = sum((r - mean) ** 2 for r in returns) / n
    std = math.sqrt(variance)
    if std == 0:
        return 0.0
    return (excess / std) * math.sqrt(252)`,
  tests: [
    {
      name: "backtest_sharpe — positive Sharpe on uptrending portfolio",
      code: `{{FUNC}}
pv = [10000, 10100, 10050, 10200, 10150, 10300]
result = backtest_sharpe(pv)
print(result > 0)`,
      expected: "True\n",
    },
    {
      name: "backtest_sharpe — correct Sharpe value",
      code: `{{FUNC}}
pv = [10000, 10100, 10050, 10200, 10150, 10300]
result = backtest_sharpe(pv)
print(round(result, 4))`,
      expected: "10.4472\n",
    },
    {
      name: "backtest_sharpe — negative Sharpe on downtrending portfolio",
      code: `{{FUNC}}
pv = [10000, 9900, 9950, 9800, 9850, 9700]
result = backtest_sharpe(pv)
print(result < 0)`,
      expected: "True\n",
    },
    {
      name: "backtest_sharpe — zero Sharpe when std is zero",
      code: `{{FUNC}}
pv = [10000, 10000, 10000, 10000, 10000]
result = backtest_sharpe(pv)
print(round(result, 4))`,
      expected: "0.0\n",
    },
  ],
};
