import type { Lesson } from "../../types";

export const realizedVolatilityLesson: Lesson = {
  id: "realized-volatility",
  title: "Realized Volatility",
  chapterId: "volatility-models",
  content: `## Realized Volatility

**Realized volatility** is a non-parametric measure of volatility computed directly from observed high-frequency price data. Unlike GARCH, it doesn't assume a parametric model.

### Definition

Given a price series, compute the log returns and take the square root of the sum of squared returns:

\`\`\`
RV = √(Σ r[t]²)
\`\`\`

where \`r[t] = ln(prices[t+1] / prices[t])\`.

### Annualization

To express realized volatility on an annualized basis, scale by the square root of the number of trading periods per year:

\`\`\`
RV_annual = RV × √(252 / n)
\`\`\`

where \`n\` is the number of return observations and 252 is the typical number of trading days per year.

### Task

Implement:
- \`realized_vol(prices)\` → square root of sum of squared log returns
- \`annualized_realized_vol(prices)\` → annualized realized volatility
`,
  starterCode: `import math

def realized_vol(prices):
    # sqrt(sum of squared log returns)
    pass

def annualized_realized_vol(prices):
    # realized_vol * sqrt(252 / n) where n = len(prices) - 1
    pass
`,
  solution: `import math

def realized_vol(prices):
    log_rets = [math.log(prices[i+1] / prices[i]) for i in range(len(prices)-1)]
    return math.sqrt(sum(r**2 for r in log_rets))

def annualized_realized_vol(prices):
    n = len(prices) - 1
    rv = realized_vol(prices)
    return rv * math.sqrt(252 / n)
`,
  tests: [
    {
      name: "realized_vol for 6 prices",
      code: `{{FUNC}}\nprices = [100.0, 101.0, 99.5, 102.0, 101.5, 103.0]\nprint(round(realized_vol(prices), 4))`,
      expected: "0.0343\n",
    },
    {
      name: "annualized_realized_vol for 6 prices",
      code: `{{FUNC}}\nprices = [100.0, 101.0, 99.5, 102.0, 101.5, 103.0]\nprint(round(annualized_realized_vol(prices), 4))`,
      expected: "0.2437\n",
    },
    {
      name: "realized_vol for 5 prices",
      code: `{{FUNC}}\nprices = [50.0, 52.0, 51.0, 53.0, 54.0]\nprint(round(realized_vol(prices), 4))`,
      expected: "0.0612\n",
    },
    {
      name: "annualized_realized_vol for 5 prices",
      code: `{{FUNC}}\nprices = [50.0, 52.0, 51.0, 53.0, 54.0]\nprint(round(annualized_realized_vol(prices), 4))`,
      expected: "0.4857\n",
    },
  ],
};
