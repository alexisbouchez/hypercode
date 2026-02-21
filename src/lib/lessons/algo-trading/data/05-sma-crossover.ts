import type { Lesson } from "../../types";

export const smaCrossover: Lesson = {
  id: "sma-crossover",
  title: "SMA Crossover Signal",
  chapterId: "trend-following",
  content: `## SMA Crossover Signal

The SMA crossover is one of the simplest and most popular trend-following signals. When a fast-moving average crosses above a slow-moving average, it signals an uptrend (buy). When it crosses below, it signals a downtrend (sell).

### Rules

Compare the fast and slow SMA at consecutive time steps:

- **Signal = 1** (bullish crossover): \`fast_sma[t-1] <= slow_sma[t-1]\` and \`fast_sma[t] > slow_sma[t]\`
- **Signal = -1** (bearish crossover): \`fast_sma[t-1] >= slow_sma[t-1]\` and \`fast_sma[t] < slow_sma[t]\`
- **Signal = 0**: otherwise (including positions where either SMA is \`None\`)

The output list has the same length as \`prices\`.

### Task

Implement \`crossover_signal(prices, fast, slow)\` that returns a list of signals (\`1\`, \`-1\`, or \`0\`) for each time step.`,
  starterCode: `def sma(prices, window):
    result = [None] * (window - 1)
    for i in range(window - 1, len(prices)):
        result.append(sum(prices[i - window + 1:i + 1]) / window)
    return result

def crossover_signal(prices, fast, slow):
    fast_sma = sma(prices, fast)
    slow_sma = sma(prices, slow)
    signals = []
    prev_fast = None
    prev_slow = None
    # TODO: iterate through SMAs and detect crossovers
    return signals`,
  solution: `def sma(prices, window):
    result = [None] * (window - 1)
    for i in range(window - 1, len(prices)):
        result.append(sum(prices[i - window + 1:i + 1]) / window)
    return result

def crossover_signal(prices, fast, slow):
    fast_sma = sma(prices, fast)
    slow_sma = sma(prices, slow)
    signals = []
    prev_fast = None
    prev_slow = None
    for i in range(len(prices)):
        if fast_sma[i] is None or slow_sma[i] is None:
            signals.append(0)
        else:
            if prev_fast is not None and prev_slow is not None:
                if prev_fast <= prev_slow and fast_sma[i] > slow_sma[i]:
                    signals.append(1)
                elif prev_fast >= prev_slow and fast_sma[i] < slow_sma[i]:
                    signals.append(-1)
                else:
                    signals.append(0)
            else:
                signals.append(0)
            prev_fast = fast_sma[i]
            prev_slow = slow_sma[i]
    return signals`,
  tests: [
    {
      name: "crossover_signal — bullish crossover at index 4",
      code: `{{FUNC}}
prices = [10, 9, 8, 9, 10, 11, 12, 11, 10, 9]
result = crossover_signal(prices, fast=2, slow=4)
print(result[4])`,
      expected: "1\n",
    },
    {
      name: "crossover_signal — bearish crossover at index 8",
      code: `{{FUNC}}
prices = [10, 9, 8, 9, 10, 11, 12, 11, 10, 9]
result = crossover_signal(prices, fast=2, slow=4)
print(result[8])`,
      expected: "-1\n",
    },
    {
      name: "crossover_signal — zero where no crossover",
      code: `{{FUNC}}
prices = [10, 9, 8, 9, 10, 11, 12, 11, 10, 9]
result = crossover_signal(prices, fast=2, slow=4)
print(result[5])`,
      expected: "0\n",
    },
    {
      name: "crossover_signal — full signal list",
      code: `{{FUNC}}
prices = [10, 9, 8, 9, 10, 11, 12, 11, 10, 9]
result = crossover_signal(prices, fast=2, slow=4)
print(result)`,
      expected: "[0, 0, 0, 0, 1, 0, 0, 0, -1, 0]\n",
    },
  ],
};
