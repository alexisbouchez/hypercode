import type { Lesson } from "../../types";

export const betaAlpha: Lesson = {
  id: "beta-alpha",
  title: "Beta & Alpha",
  chapterId: "statistical-testing",
  content: `## Beta & Alpha (CAPM)

The Capital Asset Pricing Model (CAPM) decomposes an asset's return into market exposure and excess return:

$$r_{asset} = \\alpha + \\beta \\cdot r_{market} + \\epsilon$$

**Beta** measures systematic risk — how much the asset moves with the market:
$$\\beta = \\frac{\\text{Cov}(r_{asset}, r_{market})}{\\text{Var}(r_{market})}$$

- $\\beta > 1$: more volatile than market (amplifies moves)
- $\\beta < 1$: less volatile than market (dampens moves)
- $\\beta < 0$: moves opposite to market (hedge)

**Alpha** is the excess return not explained by market exposure:
$$\\alpha = \\bar{r}_{asset} - r_f - \\beta (\\bar{r}_{market} - r_f)$$

A positive alpha means the asset outperformed the CAPM prediction — the manager "added value."

### Your Task

Implement:
- \`beta(asset_returns, market_returns)\` — CAPM beta
- \`alpha(asset_returns, market_returns, rf=0.0)\` — CAPM alpha`,
  starterCode: `def beta(asset_returns, market_returns):
    # cov(asset, market) / var(market) — use sample versions
    pass

def alpha(asset_returns, market_returns, rf=0.0):
    # mean_asset - rf - beta * (mean_market - rf)
    pass`,
  solution: `def beta(asset_returns, market_returns):
    n = len(asset_returns)
    ma = sum(asset_returns) / n
    mm = sum(market_returns) / n
    cov = sum((asset_returns[i] - ma) * (market_returns[i] - mm) for i in range(n)) / (n - 1)
    var_m = sum((r - mm) ** 2 for r in market_returns) / (n - 1)
    return cov / var_m

def alpha(asset_returns, market_returns, rf=0.0):
    n = len(asset_returns)
    ma = sum(asset_returns) / n
    mm = sum(market_returns) / n
    b = beta(asset_returns, market_returns)
    return ma - rf - b * (mm - rf)`,
  tests: [
    {
      name: "beta of correlated asset/market equals ~1.3168",
      code: `{{FUNC}}
asset = [0.01, -0.005, 0.008, 0.012, -0.003]
market = [0.008, -0.003, 0.006, 0.010, -0.002]
print(round(beta(asset, market), 4))`,
      expected: "1.3168\n",
    },
    {
      name: "alpha of correlated asset/market equals ~-0.0006",
      code: `{{FUNC}}
asset = [0.01, -0.005, 0.008, 0.012, -0.003]
market = [0.008, -0.003, 0.006, 0.010, -0.002]
print(round(alpha(asset, market), 4))`,
      expected: "-0.0006\n",
    },
    {
      name: "beta of asset identical to market equals 1.0",
      code: `{{FUNC}}
rets = [0.01, -0.005, 0.008, 0.012, -0.003]
print(round(beta(rets, rets), 4))`,
      expected: "1.0\n",
    },
  ],
};
