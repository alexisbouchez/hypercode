import type { Lesson } from "../../types";

export const drawdown: Lesson = {
  id: "drawdown",
  title: "Drawdown & Maximum Drawdown",
  chapterId: "risk-metrics",
  content: `## Drawdown & Maximum Drawdown

A **drawdown** measures the decline from a historical peak to a current value. It answers: "how far have we fallen from the highest point seen so far?"

For each price $P_i$, the drawdown is:
$$DD_i = \\frac{P_i - \\text{Peak}_i}{\\text{Peak}_i}$$

where $\\text{Peak}_i = \\max(P_0, P_1, ..., P_i)$ is the running maximum.

Drawdowns are always $\\leq 0$: zero when at a new high, negative otherwise.

**Maximum Drawdown (MDD)** is the worst (most negative) drawdown over the entire period:
$$MDD = \\min_i DD_i$$

MDD is a critical risk metric used to evaluate investment strategies. A strategy with a 50% MDD means an investor could have lost half their capital at the worst point.

### Your Task

Implement:
- \`drawdown(prices)\` — list of drawdowns from running peak
- \`max_drawdown(prices)\` — minimum drawdown value (most negative)`,
  starterCode: `def drawdown(prices):
    # For each price, compute (price - running_peak) / running_peak
    pass

def max_drawdown(prices):
    # Return the minimum value from drawdown(prices)
    pass`,
  solution: `def drawdown(prices):
    result = []
    peak = prices[0]
    for p in prices:
        if p > peak:
            peak = p
        result.append((p - peak) / peak)
    return result

def max_drawdown(prices):
    return min(drawdown(prices))`,
  tests: [
    {
      name: "drawdown([100,110,105,95,100,115,90]) correct values",
      code: `{{FUNC}}
print([round(x, 4) for x in drawdown([100, 110, 105, 95, 100, 115, 90])])`,
      expected: "[0.0, 0.0, -0.0455, -0.1364, -0.0909, 0.0, -0.2174]\n",
    },
    {
      name: "max_drawdown([100,110,105,95,100,115,90]) equals -0.2174",
      code: `{{FUNC}}
print(round(max_drawdown([100, 110, 105, 95, 100, 115, 90]), 4))`,
      expected: "-0.2174\n",
    },
    {
      name: "max_drawdown of monotonically increasing prices equals 0.0",
      code: `{{FUNC}}
print(round(max_drawdown([10, 20, 30, 40, 50]), 4))`,
      expected: "0.0\n",
    },
  ],
};
