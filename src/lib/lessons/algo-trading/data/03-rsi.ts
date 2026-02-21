import type { Lesson } from "../../types";

export const rsiLesson: Lesson = {
  id: "rsi",
  title: "RSI (Relative Strength Index)",
  chapterId: "signals",
  content: `## RSI — Relative Strength Index

The RSI is a momentum oscillator that measures the speed and magnitude of price changes. It ranges from 0 to 100. Values above 70 are typically considered overbought; values below 30 are considered oversold.

### Algorithm

For each time step \`t\` (after \`window\` prices):

1. Compute daily differences: \`diff[i] = prices[i] - prices[i-1]\`
2. Separate into gains (\`max(0, diff)\`) and losses (\`max(0, -diff)\`)
3. Compute rolling averages over the window:
   \`\`\`
   avg_gain = mean of gains in window
   avg_loss = mean of losses in window
   \`\`\`
4. RS = avg_gain / avg_loss
5. RSI = 100 − 100 / (1 + RS)

If \`avg_loss == 0\`, RSI = 100.

The first \`window\` values are \`None\` because there is insufficient data.

### Task

Implement \`rsi(prices, window=14)\` returning a list where the first \`window\` elements are \`None\` and the rest are RSI values.`,
  starterCode: `def rsi(prices, window=14):
    result = [None] * window
    gains = []
    losses = []
    for i in range(1, len(prices)):
        diff = prices[i] - prices[i - 1]
        gains.append(max(0, diff))
        losses.append(max(0, -diff))
    # TODO: compute rolling avg_gain/avg_loss and RSI for each valid window
    return result`,
  solution: `def rsi(prices, window=14):
    result = [None] * window
    gains = []
    losses = []
    for i in range(1, len(prices)):
        diff = prices[i] - prices[i - 1]
        gains.append(max(0, diff))
        losses.append(max(0, -diff))
    for i in range(window - 1, len(prices) - 1):
        w_gains = gains[i - window + 1:i + 1]
        w_losses = losses[i - window + 1:i + 1]
        avg_gain = sum(w_gains) / window
        avg_loss = sum(w_losses) / window
        if avg_loss == 0:
            result.append(100.0)
        else:
            rs = avg_gain / avg_loss
            result.append(100 - 100 / (1 + rs))
    return result`,
  tests: [
    {
      name: "rsi — first window values are None",
      code: `{{FUNC}}
result = rsi([10, 12, 11, 13, 12, 14, 13], window=3)
print(result[0] is None, result[1] is None, result[2] is None)`,
      expected: "True True True\n",
    },
    {
      name: "rsi — RSI=80 when gains dominate (window=3)",
      code: `{{FUNC}}
result = rsi([10, 12, 11, 13, 12, 14, 13], window=3)
print(round(result[3], 4))`,
      expected: "80.0\n",
    },
    {
      name: "rsi — RSI=50 when equal gains and losses (window=3)",
      code: `{{FUNC}}
result = rsi([10, 12, 11, 13, 12, 14, 13], window=3)
print(round(result[4], 4))`,
      expected: "50.0\n",
    },
    {
      name: "rsi — RSI=100 on all-gains series (window=3)",
      code: `{{FUNC}}
result = rsi([10, 11, 12, 13, 14, 15, 16], window=3)
print(round(result[3], 4))`,
      expected: "100.0\n",
    },
  ],
};
