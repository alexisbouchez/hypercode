import type { Lesson } from "../../types";

export const momentumLesson: Lesson = {
  id: "momentum",
  title: "Momentum Strategy",
  chapterId: "trend-following",
  content: `## Momentum Strategy

Momentum investing is based on the observation that assets that have performed well recently tend to continue performing well in the short term (and vice versa).

### Momentum

The momentum at time \`t\` with lookback period \`lb\` is the return over the lookback window:

\`\`\`
momentum[t] = prices[t] / prices[t - lb] - 1
\`\`\`

The first \`lb\` values are \`None\` because there are not enough prior prices.

### Momentum Signal

- **+1** if momentum > 0 (upward trend, go long)
- **-1** if momentum < 0 (downward trend, go short)
- **0** if momentum == 0 or None

### Task

Implement:
- \`momentum(prices, lookback)\` — returns the momentum series (first \`lookback\` values are \`None\`)
- \`momentum_signal(prices, lookback)\` — returns \`1\`, \`-1\`, or \`0\` for each time step`,
  starterCode: `def momentum(prices, lookback):
    result = [None] * lookback
    # TODO: compute return over lookback period for each valid index
    return result

def momentum_signal(prices, lookback):
    mom = momentum(prices, lookback)
    result = []
    # TODO: map momentum values to 1, -1, or 0
    return result`,
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
    return result`,
  tests: [
    {
      name: "momentum — first lookback values are None",
      code: `{{FUNC}}
result = momentum([100, 102, 101, 105, 103, 108], 2)
print(result[0] is None, result[1] is None)`,
      expected: "True True\n",
    },
    {
      name: "momentum — correct value at index 2",
      code: `{{FUNC}}
result = momentum([100, 102, 101, 105, 103, 108], 2)
print(round(result[2], 4))`,
      expected: "0.01\n",
    },
    {
      name: "momentum_signal — positive momentum gives 1",
      code: `{{FUNC}}
result = momentum_signal([100, 102, 101, 105, 103, 108], 2)
print(result[2], result[3], result[4], result[5])`,
      expected: "1 1 1 1\n",
    },
    {
      name: "momentum_signal — negative momentum gives -1",
      code: `{{FUNC}}
result = momentum_signal([100, 102, 99, 97, 100, 95], 2)
print(result[2])`,
      expected: "-1\n",
    },
  ],
};
