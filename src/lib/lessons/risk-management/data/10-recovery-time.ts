import type { Lesson } from "../../types";

export const recoveryTime: Lesson = {
  id: "recovery-time",
  title: "Recovery Time",
  chapterId: "tail-risk",
  content: `## Recovery Time

**Recovery time** measures how many periods it takes to climb back to the pre-drawdown peak after the maximum drawdown trough. It is an important complement to max drawdown: a deep but quick recovery is very different from a shallow but prolonged one.

### Algorithm

1. Compute drawdowns to find the maximum drawdown value
2. Find the **trough index** (where the max drawdown occurs)
3. Before the trough, find the **peak index** (the last point achieving that high-water mark)
4. Scan forward from the trough to find the first index where \`equity >= peak_value\`
5. Recovery time = that index − peak index
6. If never recovered, return **−1**

### Example

Equity: \`[100, 110, 105, 95, 100, 108, 115]\`  
- Peak = 110 at index 1  
- Trough = 95 at index 3  
- Recovery: index 6 (value 115 ≥ 110)  
- Recovery time = 6 − 1 = **5**

Equity: \`[100, 120, 100, 80, 90, 110]\`  
- Peak = 120 at index 1; never recovers → **−1**
`,
  starterCode: `def recovery_time(equity_curve):
    pass
`,
  solution: `def recovery_time(equity_curve):
    # Find drawdowns to locate max drawdown
    peak = equity_curve[0]
    dds = []
    for val in equity_curve:
        if val > peak:
            peak = val
        dds.append((val - peak) / peak)
    max_dd = min(dds)
    trough_idx = dds.index(max_dd)
    # Find the peak before the trough
    peak_val = equity_curve[0]
    peak_idx = 0
    for i in range(trough_idx + 1):
        if equity_curve[i] > peak_val:
            peak_val = equity_curve[i]
            peak_idx = i
    # Find first recovery point after trough
    for i in range(trough_idx + 1, len(equity_curve)):
        if equity_curve[i] >= peak_val:
            return i - peak_idx
    return -1
`,
  tests: [
    {
      name: "recovery_time with full recovery",
      code: `{{FUNC}}\nprint(recovery_time([100, 110, 105, 95, 100, 108, 115]))`,
      expected: "5\n",
    },
    {
      name: "recovery_time never recovers returns -1",
      code: `{{FUNC}}\nprint(recovery_time([100, 120, 100, 80, 90, 110]))`,
      expected: "-1\n",
    },
    {
      name: "recovery_time quick recovery",
      code: `{{FUNC}}\nprint(recovery_time([100, 90, 95, 102, 105]))`,
      expected: "3\n",
    },
    {
      name: "recovery_time gradual recovery after peak",
      code: `{{FUNC}}\nprint(recovery_time([100, 120, 90, 95, 105, 115, 122]))`,
      expected: "5\n",
    },
  ],
};
