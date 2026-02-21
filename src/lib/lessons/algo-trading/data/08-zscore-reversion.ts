import type { Lesson } from "../../types";

export const zscoreReversion: Lesson = {
  id: "zscore-reversion",
  title: "Z-Score Mean Reversion",
  chapterId: "trend-following",
  content: `## Z-Score Mean Reversion

The z-score measures how many standard deviations a value is from the rolling mean. It is widely used in mean-reversion strategies: if a price is far above its recent average (high z-score), it may revert downward; if it is far below (low z-score), it may revert upward.

### Rolling Z-Score

For a window \`w\` ending at index \`t\`:

\`\`\`
mean = sum(xs[t-w+1:t+1]) / w
std  = sqrt(sum((x - mean)^2 for x in window) / w)   # population std
z    = (xs[t] - mean) / std
\`\`\`

If \`std == 0\`, return \`0.0\` to avoid division by zero.

The first \`window - 1\` values are \`None\`.

### Task

Implement \`zscore(xs, window)\` that returns the rolling z-score list.`,
  starterCode: `import math

def zscore(xs, window):
    result = [None] * (window - 1)
    for i in range(window - 1, len(xs)):
        w = xs[i - window + 1:i + 1]
        mean = sum(w) / window
        variance = sum((x - mean) ** 2 for x in w) / window
        std = math.sqrt(variance)
        # TODO: append z-score (handle std == 0)
    return result`,
  solution: `import math

def zscore(xs, window):
    result = [None] * (window - 1)
    for i in range(window - 1, len(xs)):
        w = xs[i - window + 1:i + 1]
        mean = sum(w) / window
        variance = sum((x - mean) ** 2 for x in w) / window
        std = math.sqrt(variance)
        if std == 0:
            result.append(0.0)
        else:
            result.append((xs[i] - mean) / std)
    return result`,
  tests: [
    {
      name: "zscore — first window-1 values are None",
      code: `{{FUNC}}
result = zscore([10, 9, 11, 10, 8, 12, 10], 3)
print(result[0] is None, result[1] is None)`,
      expected: "True True\n",
    },
    {
      name: "zscore — positive z when value above mean",
      code: `{{FUNC}}
result = zscore([10, 9, 11, 10, 8, 12, 10], 3)
print(round(result[2], 4))`,
      expected: "1.2247\n",
    },
    {
      name: "zscore — zero z when value equals mean",
      code: `{{FUNC}}
result = zscore([10, 9, 11, 10, 8, 12, 10], 3)
print(round(result[3], 4))`,
      expected: "0.0\n",
    },
    {
      name: "zscore — negative z when value below mean",
      code: `{{FUNC}}
result = zscore([10, 9, 11, 10, 8, 12, 10], 3)
print(round(result[4], 4))`,
      expected: "-1.3363\n",
    },
  ],
};
