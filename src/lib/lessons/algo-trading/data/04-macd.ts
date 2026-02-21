import type { Lesson } from "../../types";

export const macdLesson: Lesson = {
  id: "macd",
  title: "MACD",
  chapterId: "signals",
  content: `## MACD — Moving Average Convergence Divergence

MACD is a trend-following momentum indicator. It consists of three components:

### Components

**MACD Line**: Difference between a fast and slow EMA:
\`\`\`
macd_line[t] = EMA(fast)[t] - EMA(slow)[t]
\`\`\`

**Signal Line**: EMA of the MACD line with a signal period:
\`\`\`
signal[t] = EMA(macd_line, signal_period)[t]
\`\`\`

**Histogram**: Difference between the MACD line and signal line:
\`\`\`
histogram[t] = macd_line[t] - signal[t]
\`\`\`

### EMA Formula

Use \`alpha = 2 / (period + 1)\` and the standard EMA recurrence starting from \`prices[0]\`.

### Typical Parameters

- Fast: 12 periods
- Slow: 26 periods
- Signal: 9 periods

### Task

Implement:
- \`macd_line(prices, fast=12, slow=26)\`
- \`macd_signal(prices, fast=12, slow=26, signal=9)\`
- \`macd_histogram(prices, fast=12, slow=26, signal=9)\``,
  starterCode: `def ema_list(prices, period):
    alpha = 2.0 / (period + 1)
    result = [prices[0]]
    for t in range(1, len(prices)):
        result.append(alpha * prices[t] + (1 - alpha) * result[-1])
    return result

def macd_line(prices, fast=12, slow=26):
    # TODO: return EMA(fast) - EMA(slow) element-wise
    pass

def macd_signal(prices, fast=12, slow=26, signal=9):
    # TODO: return EMA of macd_line with signal period
    pass

def macd_histogram(prices, fast=12, slow=26, signal=9):
    # TODO: return macd_line - macd_signal element-wise
    pass`,
  solution: `def ema_list(prices, period):
    alpha = 2.0 / (period + 1)
    result = [prices[0]]
    for t in range(1, len(prices)):
        result.append(alpha * prices[t] + (1 - alpha) * result[-1])
    return result

def macd_line(prices, fast=12, slow=26):
    e_fast = ema_list(prices, fast)
    e_slow = ema_list(prices, slow)
    return [f - s for f, s in zip(e_fast, e_slow)]

def macd_signal(prices, fast=12, slow=26, signal=9):
    ml = macd_line(prices, fast, slow)
    return ema_list(ml, signal)

def macd_histogram(prices, fast=12, slow=26, signal=9):
    ml = macd_line(prices, fast, slow)
    sig = ema_list(ml, signal)
    return [m - s for m, s in zip(ml, sig)]`,
  tests: [
    {
      name: "macd_line — zero when fast equals slow (same period)",
      code: `{{FUNC}}
result = macd_line([10, 11, 12, 11, 10, 11, 12, 13, 14, 15], fast=3, slow=5)
print(round(result[0], 4))`,
      expected: "0\n",
    },
    {
      name: "macd_line — last value with fast=3, slow=5",
      code: `{{FUNC}}
result = macd_line([10, 11, 12, 11, 10, 11, 12, 13, 14, 15], fast=3, slow=5)
print(round(result[-1], 4))`,
      expected: "0.7042\n",
    },
    {
      name: "macd_signal — last value with fast=3, slow=5, signal=3",
      code: `{{FUNC}}
result = macd_signal([10, 11, 12, 11, 10, 11, 12, 13, 14, 15], fast=3, slow=5, signal=3)
print(round(result[-1], 4))`,
      expected: "0.5675\n",
    },
    {
      name: "macd_histogram — last value equals line minus signal",
      code: `{{FUNC}}
prices = [10, 11, 12, 11, 10, 11, 12, 13, 14, 15]
result = macd_histogram(prices, fast=3, slow=5, signal=3)
print(round(result[-1], 4))`,
      expected: "0.1367\n",
    },
  ],
};
