import type { Lesson } from "../../types";

export const pairsTrading: Lesson = {
  id: "pairs-trading",
  title: "Pairs Trading (Spread & Entry)",
  chapterId: "mean-reversion",
  content: `## Pairs Trading

Pairs trading is a market-neutral strategy that exploits the co-movement of two related assets. When the spread between them deviates from its historical norm, you bet on reversion.

### Spread

The spread is the difference between two price series, adjusted by a **hedge ratio**:

\`\`\`
spread[i] = series_a[i] - hedge_ratio * series_b[i]
\`\`\`

### Entry Signal

To generate signals, compute the z-score of the entire spread series and use an entry threshold:

\`\`\`
mean  = mean(spread)
std   = population std(spread)
entry_z = entry_threshold * std
\`\`\`

- **+1** (short spread): \`spread[i] > entry_z\` (spread too wide, expect reversion)
- **-1** (long spread): \`spread[i] < -entry_z\` (spread too narrow, expect expansion)
- **0**: otherwise

### Task

Implement:
- \`spread(series_a, series_b, hedge_ratio)\`
- \`pairs_signal(spread_series, entry_threshold=1.5)\``,
  starterCode: `import math

def spread(series_a, series_b, hedge_ratio):
    # TODO: return series_a[i] - hedge_ratio * series_b[i] for each i
    pass

def pairs_signal(spread_series, entry_threshold=1.5):
    n = len(spread_series)
    mean = sum(spread_series) / n
    variance = sum((x - mean) ** 2 for x in spread_series) / n
    std = math.sqrt(variance)
    entry_z = entry_threshold * std
    result = []
    # TODO: assign 1, -1, or 0 based on spread vs entry_z
    return result`,
  solution: `import math

def spread(series_a, series_b, hedge_ratio):
    return [a - hedge_ratio * b for a, b in zip(series_a, series_b)]

def pairs_signal(spread_series, entry_threshold=1.5):
    n = len(spread_series)
    mean = sum(spread_series) / n
    variance = sum((x - mean) ** 2 for x in spread_series) / n
    std = math.sqrt(variance)
    entry_z = entry_threshold * std
    result = []
    for s in spread_series:
        if s > entry_z:
            result.append(1)
        elif s < -entry_z:
            result.append(-1)
        else:
            result.append(0)
    return result`,
  tests: [
    {
      name: "spread — basic calculation with hedge_ratio=1.0",
      code: `{{FUNC}}
a = [10, 11, 9, 13, 10]
b = [9, 9, 10, 9, 10]
result = spread(a, b, 1.0)
print([round(x, 4) for x in result])`,
      expected: "[1.0, 2.0, -1.0, 4.0, 0.0]\n",
    },
    {
      name: "spread — hedge_ratio scales correctly",
      code: `{{FUNC}}
a = [20, 22, 21, 24, 20]
b = [10, 11, 10, 12, 10]
result = spread(a, b, 2.0)
print([round(x, 4) for x in result])`,
      expected: "[0.0, 0.0, 1.0, 0.0, 0.0]\n",
    },
    {
      name: "pairs_signal — entry when spread far above threshold",
      code: `{{FUNC}}
sp = [1.0, 2.0, -1.0, 4.0, 0.0]
result = pairs_signal(sp, entry_threshold=1.0)
print(result)`,
      expected: "[0, 1, 0, 1, 0]\n",
    },
    {
      name: "pairs_signal — higher threshold reduces signals",
      code: `{{FUNC}}
sp = [1.0, 2.0, -1.0, 4.0, 0.0]
result = pairs_signal(sp, entry_threshold=2.0)
print(result)`,
      expected: "[0, 0, 0, 1, 0]\n",
    },
  ],
};
