import type { Lesson } from "../../types";

export const bollingerBands: Lesson = {
  id: "bollinger-bands",
  title: "Bollinger Bands",
  chapterId: "signals",
  content: `## Bollinger Bands

Bollinger Bands are a volatility indicator consisting of three lines:
- **Middle band**: Simple Moving Average (SMA) over a rolling window
- **Upper band**: Middle + \`n_std\` standard deviations
- **Lower band**: Middle − \`n_std\` standard deviations

They help identify overbought or oversold conditions. When prices touch the upper band, the asset may be overbought; when they touch the lower band, it may be oversold.

### Standard Deviation (Population)

For a window \`w\` ending at index \`t\`:

\`\`\`
mean = sum(prices[t-w+1:t+1]) / w
std  = sqrt(sum((p - mean)^2 for p in window) / w)
\`\`\`

### Task

Implement \`bollinger_bands(prices, window, n_std=2.0)\` that returns a tuple \`(upper, middle, lower)\`, each a list of the same length as \`prices\`. The first \`window - 1\` elements of each list are \`None\`.`,
  starterCode: `import math

def bollinger_bands(prices, window, n_std=2.0):
    upper = [None] * (window - 1)
    middle = [None] * (window - 1)
    lower = [None] * (window - 1)
    # TODO: compute rolling mean and std, then upper/lower bands
    return upper, middle, lower`,
  solution: `import math

def bollinger_bands(prices, window, n_std=2.0):
    upper = [None] * (window - 1)
    middle = [None] * (window - 1)
    lower = [None] * (window - 1)
    for i in range(window - 1, len(prices)):
        window_prices = prices[i - window + 1:i + 1]
        m = sum(window_prices) / window
        variance = sum((x - m) ** 2 for x in window_prices) / window
        std = math.sqrt(variance)
        middle.append(m)
        upper.append(m + n_std * std)
        lower.append(m - n_std * std)
    return upper, middle, lower`,
  tests: [
    {
      name: "bollinger_bands — first window-1 values are None",
      code: `{{FUNC}}
u, m, lo = bollinger_bands([10, 11, 12, 13, 14, 15], 3)
print(u[0] is None, u[1] is None, m[0] is None, lo[0] is None)`,
      expected: "True True True True\n",
    },
    {
      name: "bollinger_bands — middle equals SMA",
      code: `{{FUNC}}
u, m, lo = bollinger_bands([10, 11, 12, 13, 14, 15], 3)
print([round(x, 4) if x is not None else None for x in m])`,
      expected: "[None, None, 11.0, 12.0, 13.0, 14.0]\n",
    },
    {
      name: "bollinger_bands — upper > middle > lower at index 3",
      code: `{{FUNC}}
u, m, lo = bollinger_bands([10, 11, 12, 13, 14, 15], 3)
print(u[3] > m[3] > lo[3])`,
      expected: "True\n",
    },
    {
      name: "bollinger_bands — correct values at index 3",
      code: `{{FUNC}}
u, m, lo = bollinger_bands([10, 11, 12, 13, 14, 15], 3)
print(round(u[3], 4), round(m[3], 4), round(lo[3], 4))`,
      expected: "13.633 12.0 10.367\n",
    },
  ],
};
