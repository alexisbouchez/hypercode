import type { Lesson } from "../../types";

export const cvar: Lesson = {
  id: "cvar",
  title: "Conditional VaR (CVaR / Expected Shortfall)",
  chapterId: "market-risk",
  content: `## Conditional VaR (CVaR / Expected Shortfall)

VaR tells you the threshold loss at a confidence level, but not *how bad* things get beyond that threshold. **CVaR** (also called **Expected Shortfall**) fills this gap by averaging all losses that exceed the VaR.

### Formula

\`\`\`
CVaR = -mean(returns that fall in the tail)
\`\`\`

Concretely, using historical returns:
1. Sort returns ascending
2. idx = \`int((1 - confidence) × n)\`
3. Tail = \`sorted_returns[:idx]\`
4. If idx == 0 (very small sample), use just the worst return: \`[sorted_returns[0]]\`
5. CVaR = \`-mean(tail)\`

### CVaR vs VaR

| Metric | Meaning |
|--------|---------|
| VaR 95% | Worst loss exceeded only 5% of the time |
| CVaR 95% | Average loss in that worst 5% |

CVaR is a **coherent risk measure** (satisfies sub-additivity), whereas VaR is not.

### Example

20 returns at 85% confidence:  
idx = \`int(0.15 × 20) = 3\`  
tail = 3 worst returns → CVaR = average of those 3
`,
  starterCode: `def cvar(returns, confidence=0.95):
    pass
`,
  solution: `def cvar(returns, confidence=0.95):
    sorted_r = sorted(returns)
    idx = int((1 - confidence) * len(sorted_r))
    tail = sorted_r[:idx] if idx > 0 else [sorted_r[0]]
    return -sum(tail) / len(tail)
`,
  tests: [
    {
      name: "cvar at 95% confidence (small sample — uses worst return)",
      code: `{{FUNC}}\nprint(round(cvar([-0.05, -0.03, -0.01, 0.01, 0.02, 0.03, 0.04], 0.95), 4))`,
      expected: "0.05\n",
    },
    {
      name: "cvar at 95% confidence (10 returns)",
      code: `{{FUNC}}\nprint(round(cvar([-0.10, -0.08, -0.05, -0.03, -0.01, 0.01, 0.02, 0.03, 0.04, 0.05], 0.95), 4))`,
      expected: "0.1\n",
    },
    {
      name: "cvar at 95% confidence (20 returns)",
      code: `{{FUNC}}\nreturns = [-0.12, -0.09, -0.07, -0.05, -0.03, -0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12, 0.13, 0.14]\nprint(round(cvar(returns, 0.95), 4))`,
      expected: "0.12\n",
    },
    {
      name: "cvar at 85% confidence (20 returns) — averages 3 worst",
      code: `{{FUNC}}\nreturns = [-0.12, -0.09, -0.07, -0.05, -0.03, -0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12, 0.13, 0.14]\nprint(round(cvar(returns, 0.85), 4))`,
      expected: "0.0933\n",
    },
  ],
};
