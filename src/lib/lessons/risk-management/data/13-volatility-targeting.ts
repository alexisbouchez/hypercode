import type { Lesson } from "../../types";

export const volatilityTargeting: Lesson = {
  id: "volatility-targeting",
  title: "Volatility Targeting",
  chapterId: "position-management",
  content: `## Volatility Targeting

**Volatility targeting** dynamically adjusts position size so that the portfolio maintains a constant level of risk regardless of changing market conditions. When markets become more volatile, you scale down; when volatility is low, you scale up.

### Formula

\`\`\`
position_size = capital × (target_vol / realized_vol)
\`\`\`

Where:
- \`target_vol\` — desired annualized volatility (e.g., 0.10 = 10%)
- \`realized_vol\` — current measured volatility (e.g., from recent returns)
- \`capital\` — total allocated capital

### Properties

- If \`realized_vol > target_vol\`: position is scaled **below** capital (risk-off)
- If \`realized_vol < target_vol\`: position is scaled **above** capital (risk-on, can use leverage)
- If \`realized_vol == target_vol\`: fully invested

### Example

target_vol = 10%, realized_vol = 15%, capital = $1,000,000  
position = 1,000,000 × (0.10 / 0.15) = **$666,666.67**

The portfolio is scaled to 66.7% invested to bring risk back to target.
`,
  starterCode: `def vol_target_size(target_vol, realized_vol, capital):
    pass
`,
  solution: `def vol_target_size(target_vol, realized_vol, capital):
    return capital * target_vol / realized_vol
`,
  tests: [
    {
      name: "vol_target_size target=10% realized=15% capital=1M",
      code: `{{FUNC}}\nprint(round(vol_target_size(0.10, 0.15, 1000000), 4))`,
      expected: "666666.6667\n",
    },
    {
      name: "vol_target_size target=15% realized=20% capital=500k",
      code: `{{FUNC}}\nprint(round(vol_target_size(0.15, 0.20, 500000), 4))`,
      expected: "375000.0\n",
    },
    {
      name: "vol_target_size target=10% realized=25% capital=1M",
      code: `{{FUNC}}\nprint(round(vol_target_size(0.10, 0.25, 1000000), 4))`,
      expected: "400000.0\n",
    },
    {
      name: "vol_target_size target equals realized — full capital",
      code: `{{FUNC}}\nprint(round(vol_target_size(0.20, 0.20, 750000), 4))`,
      expected: "750000.0\n",
    },
  ],
};
