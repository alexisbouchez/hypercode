import type { Lesson } from "../../types";

export const trendFollowingReturns: Lesson = {
  id: "trend-following-returns",
  title: "Trend Following Returns",
  chapterId: "trend-following",
  content: `## Trend Following Returns

Once we have a momentum signal, we can compute the strategy's returns. The idea is simple: trade in the direction of the signal, entering positions based on the prior day's signal.

### Algorithm

For each day \`t\` from 1 to \`len(prices) - 1\`:

1. Compute the daily return: \`daily_ret = prices[t] / prices[t-1] - 1\`
2. Read the signal from the previous day: \`sig = signals[t-1]\`
3. Strategy return: \`sig * daily_ret\`

This means:
- If the signal was \`1\` (long), we earn the daily return.
- If the signal was \`-1\` (short), we earn the negative of the daily return.
- If the signal was \`0\` (flat), we earn nothing.

The result list has length \`len(prices) - 1\`.

### Task

Implement \`trend_returns(prices, lookback)\` using the momentum signal from the previous lesson.`,
  starterCode: `def momentum(prices, lookback):
    result = [None] * lookback
    for t in range(lookback, len(prices)):
        result.append(prices[t] / prices[t - lookback] - 1)
    return result

def momentum_signal(prices, lookback):
    mom = momentum(prices, lookback)
    result = []
    for m in mom:
        if m is None:
            result.append(0)
        elif m > 0:
            result.append(1)
        elif m < 0:
            result.append(-1)
        else:
            result.append(0)
    return result

def trend_returns(prices, lookback):
    signals = momentum_signal(prices, lookback)
    returns = []
    # TODO: compute strategy daily returns using prior-day signal
    return returns`,
  solution: `def momentum(prices, lookback):
    result = [None] * lookback
    for t in range(lookback, len(prices)):
        result.append(prices[t] / prices[t - lookback] - 1)
    return result

def momentum_signal(prices, lookback):
    mom = momentum(prices, lookback)
    result = []
    for m in mom:
        if m is None:
            result.append(0)
        elif m > 0:
            result.append(1)
        elif m < 0:
            result.append(-1)
        else:
            result.append(0)
    return result

def trend_returns(prices, lookback):
    signals = momentum_signal(prices, lookback)
    returns = []
    for t in range(1, len(prices)):
        daily_ret = prices[t] / prices[t - 1] - 1
        sig = signals[t - 1]
        returns.append(sig * daily_ret)
    return returns`,
  tests: [
    {
      name: "trend_returns — length is len(prices) - 1",
      code: `{{FUNC}}
result = trend_returns([100, 102, 104, 103, 106, 108], 2)
print(len(result))`,
      expected: "5\n",
    },
    {
      name: "trend_returns — zero return when signal is flat",
      code: `{{FUNC}}
result = trend_returns([100, 102, 104, 103, 106, 108], 2)
print(round(result[0], 6), round(result[1], 6))`,
      expected: "0.0 0.0\n",
    },
    {
      name: "trend_returns — negative return when long into a down day",
      code: `{{FUNC}}
result = trend_returns([100, 102, 104, 103, 106, 108], 2)
print(round(result[2], 6))`,
      expected: "-0.009615\n",
    },
    {
      name: "trend_returns — positive return when long into an up day",
      code: `{{FUNC}}
result = trend_returns([100, 102, 104, 103, 106, 108], 2)
print(round(result[3], 6))`,
      expected: "0.029126\n",
    },
  ],
};
