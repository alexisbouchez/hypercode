import type { Lesson } from "../../types";

export const historicalVolatility: Lesson = {
  id: "historical-volatility",
  title: "Historical Volatility",
  chapterId: "risk-metrics",
  content: `## Historical Volatility

**Volatility** is the standard deviation of returns. It is the most common measure of risk in finance.

**Historical volatility** is estimated from past price data using log returns:

1. Compute log returns: $r_i = \\ln(P_i / P_{i-1})$
2. Compute the **sample standard deviation** of these returns
3. **Annualize**: multiply by $\\sqrt{252}$ (trading days per year)

$$\\sigma_{annual} = \\sigma_{daily} \\times \\sqrt{252}$$

Annualizing lets you compare volatility across assets regardless of how frequently the data is sampled.

**Daily volatility** (without annualization) is useful for short-term risk assessment.

### Your Task

Implement:
- \`hist_volatility_daily(prices)\` — sample std of log returns (daily)
- \`hist_volatility(prices)\` — annualized volatility (daily std × √252)`,
  starterCode: `import math

def hist_volatility_daily(prices):
    # Compute log returns, then sample std
    pass

def hist_volatility(prices):
    # Annualize: hist_volatility_daily * sqrt(252)
    pass`,
  solution: `import math

def hist_volatility_daily(prices):
    log_rets = [math.log(prices[i] / prices[i - 1]) for i in range(1, len(prices))]
    n = len(log_rets)
    m = sum(log_rets) / n
    return math.sqrt(sum((r - m) ** 2 for r in log_rets) / (n - 1))

def hist_volatility(prices):
    return hist_volatility_daily(prices) * math.sqrt(252)`,
  tests: [
    {
      name: "hist_volatility_daily for [100,102,101,103,105,104] equals ~0.016",
      code: `{{FUNC}}
print(round(hist_volatility_daily([100, 102, 101, 103, 105, 104]), 4))`,
      expected: "0.016\n",
    },
    {
      name: "hist_volatility (annualized) equals ~0.2544",
      code: `{{FUNC}}
print(round(hist_volatility([100, 102, 101, 103, 105, 104]), 4))`,
      expected: "0.2544\n",
    },
    {
      name: "constant prices have zero volatility",
      code: `{{FUNC}}
print(round(hist_volatility_daily([100, 100, 100, 100]), 4))`,
      expected: "0.0\n",
    },
  ],
};
