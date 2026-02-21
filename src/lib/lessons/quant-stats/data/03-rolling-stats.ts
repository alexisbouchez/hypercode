import type { Lesson } from "../../types";

export const rollingStats: Lesson = {
  id: "rolling-stats",
  title: "Rolling Statistics",
  chapterId: "return-analysis",
  content: `## Rolling Statistics

A **rolling window** calculation computes a statistic over a sliding fixed-size window of data. This is essential for detecting trends, smoothing noise, and measuring recent volatility in time series.

For a window of size $w$ applied to a series $x_1, x_2, ..., x_n$:
- Positions 0 through $w-2$ have fewer than $w$ values available → output \`None\`
- Position $i \\geq w-1$ uses values $x_{i-w+1}$ through $x_i$

**Rolling Mean** at position $i$:
$$\\bar{x}_i = \\frac{1}{w} \\sum_{j=i-w+1}^{i} x_j$$

**Rolling Std** at position $i$ (sample standard deviation):
$$s_i = \\sqrt{\\frac{1}{w-1} \\sum_{j=i-w+1}^{i} (x_j - \\bar{x}_i)^2}$$

### Your Task

Implement:
- \`rolling_mean(xs, window)\` — list of rolling means, \`None\` for first \`window-1\` positions
- \`rolling_std(xs, window)\` — list of rolling sample stds, \`None\` for first \`window-1\` positions`,
  starterCode: `import math

def rolling_mean(xs, window):
    # Return list of rolling means, None for first window-1 positions
    pass

def rolling_std(xs, window):
    # Return list of rolling sample stds, None for first window-1 positions
    pass`,
  solution: `import math

def rolling_mean(xs, window):
    result = []
    for i in range(len(xs)):
        if i < window - 1:
            result.append(None)
        else:
            result.append(sum(xs[i - window + 1:i + 1]) / window)
    return result

def rolling_std(xs, window):
    result = []
    for i in range(len(xs)):
        if i < window - 1:
            result.append(None)
        else:
            w = xs[i - window + 1:i + 1]
            m = sum(w) / window
            std = math.sqrt(sum((x - m) ** 2 for x in w) / (window - 1))
            result.append(round(std, 4))
    return result`,
  tests: [
    {
      name: "rolling_mean([1,2,3,4,5], 3) pads with None",
      code: `{{FUNC}}
print(rolling_mean([1,2,3,4,5], 3))`,
      expected: "[None, None, 2.0, 3.0, 4.0]\n",
    },
    {
      name: "rolling_std([1,2,3,4,5], 3) equals 1.0 for each valid window",
      code: `{{FUNC}}
print(rolling_std([1,2,3,4,5], 3))`,
      expected: "[None, None, 1.0, 1.0, 1.0]\n",
    },
    {
      name: "rolling_mean with window=2",
      code: `{{FUNC}}
print(rolling_mean([10,20,30,40], 2))`,
      expected: "[None, 15.0, 25.0, 35.0]\n",
    },
  ],
};
