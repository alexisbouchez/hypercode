import type { Lesson } from "../../types";

export const historicalVar: Lesson = {
  id: "historical-var",
  title: "Historical VaR",
  chapterId: "market-risk",
  content: `## Historical VaR

**Value at Risk (VaR)** answers: *"What is the most I can lose over a given period at a given confidence level?"*

The **historical simulation** approach uses actual past returns — no distributional assumptions needed. You simply sort your return series and read off the appropriate percentile.

### Algorithm

Given a list of returns and a confidence level \`c\`:
1. Sort returns in ascending order
2. Compute the index: \`idx = int((1 - c) * n)\`
3. The VaR is the **negative** of the return at that index (so VaR is expressed as a positive loss)

For 95% confidence on 7 observations, \`idx = int(0.05 * 7) = 0\`, so we take the worst return.

### Formula

\`\`\`
VaR = -sorted_returns[int((1 - confidence) * n)]
\`\`\`

### Example

Returns: \`[-0.05, -0.03, -0.01, 0.01, 0.02, 0.03, 0.04]\`  
Sorted (already sorted), 95% confidence:  
idx = 0 → worst return = -0.05 → **VaR = 0.05 (5%)**
`,
  starterCode: `def historical_var(returns, confidence=0.95):
    pass
`,
  solution: `def historical_var(returns, confidence=0.95):
    sorted_r = sorted(returns)
    idx = int((1 - confidence) * len(sorted_r))
    return -sorted_r[idx]
`,
  tests: [
    {
      name: "historical_var at 95% confidence (small sample)",
      code: `{{FUNC}}\nprint(round(historical_var([-0.05, -0.03, -0.01, 0.01, 0.02, 0.03, 0.04], 0.95), 4))`,
      expected: "0.05\n",
    },
    {
      name: "historical_var at 90% confidence (small sample)",
      code: `{{FUNC}}\nprint(round(historical_var([-0.05, -0.03, -0.01, 0.01, 0.02, 0.03, 0.04], 0.90), 4))`,
      expected: "0.05\n",
    },
    {
      name: "historical_var at 95% confidence (10 returns)",
      code: `{{FUNC}}\nprint(round(historical_var([-0.10, -0.08, -0.05, -0.02, 0.01, 0.03, 0.05, 0.07, 0.09, 0.12], 0.95), 4))`,
      expected: "0.1\n",
    },
    {
      name: "historical_var at 90% confidence (10 returns)",
      code: `{{FUNC}}\nprint(round(historical_var([-0.10, -0.08, -0.05, -0.02, 0.01, 0.03, 0.05, 0.07, 0.09, 0.12, 0.14], 0.90), 4))`,
      expected: "0.08\n",
    },
  ],
};
