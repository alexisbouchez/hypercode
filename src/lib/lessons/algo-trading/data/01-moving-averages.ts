import type { Lesson } from "../../types";

export const movingAverages: Lesson = {
  id: "moving-averages",
  title: "Simple & Exponential Moving Averages",
  chapterId: "signals",
  content: `## Simple & Exponential Moving Averages

Moving averages smooth out price data to identify trends. They are the foundation of most technical indicators.

### Simple Moving Average (SMA)

The SMA over a window of size \`w\` at time \`t\` is the arithmetic mean of the last \`w\` prices:

\`\`\`
SMA[t] = (prices[t] + prices[t-1] + ... + prices[t-w+1]) / w
\`\`\`

The first \`w - 1\` values are \`None\` because there is not enough data yet.

### Exponential Moving Average (EMA)

The EMA applies an exponentially decaying weight to past prices using a smoothing factor \`alpha\`:

\`\`\`
EMA[0] = prices[0]
EMA[t] = alpha * prices[t] + (1 - alpha) * EMA[t-1]
\`\`\`

Higher \`alpha\` gives more weight to recent prices.

### Task

Implement:
- \`sma(prices, window)\` — returns a list where the first \`window - 1\` elements are \`None\` and subsequent elements are the rolling mean.
- \`ema(prices, alpha)\` — returns the exponential moving average starting from \`prices[0]\`.`,
  starterCode: `def sma(prices, window):
    result = [None] * (window - 1)
    # TODO: append rolling mean for each valid window
    return result

def ema(prices, alpha):
    result = [prices[0]]
    # TODO: apply EMA formula for each subsequent price
    return result`,
  solution: `def sma(prices, window):
    result = [None] * (window - 1)
    for i in range(window - 1, len(prices)):
        result.append(sum(prices[i - window + 1:i + 1]) / window)
    return result

def ema(prices, alpha):
    result = [prices[0]]
    for t in range(1, len(prices)):
        result.append(alpha * prices[t] + (1 - alpha) * result[-1])
    return result`,
  tests: [
    {
      name: "sma([10,11,12,13,14,15], 3) — first two are None",
      code: `{{FUNC}}
result = sma([10, 11, 12, 13, 14, 15], 3)
print(result[0] is None, result[1] is None)`,
      expected: "True True\n",
    },
    {
      name: "sma([10,11,12,13,14,15], 3) — rolling values",
      code: `{{FUNC}}
result = sma([10, 11, 12, 13, 14, 15], 3)
print([round(x, 4) if x is not None else None for x in result])`,
      expected: "[None, None, 11.0, 12.0, 13.0, 14.0]\n",
    },
    {
      name: "ema([10,11,12,13,14,15], 0.3) — first value equals prices[0]",
      code: `{{FUNC}}
result = ema([10, 11, 12, 13, 14, 15], 0.3)
print(round(result[0], 4))`,
      expected: "10\n",
    },
    {
      name: "ema([10,11,12,13,14,15], 0.3) — last value",
      code: `{{FUNC}}
result = ema([10, 11, 12, 13, 14, 15], 0.3)
print(round(result[-1], 4))`,
      expected: "13.0588\n",
    },
  ],
};
