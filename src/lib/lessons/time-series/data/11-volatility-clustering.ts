import type { Lesson } from "../../types";

export const volatilityClusteringLesson: Lesson = {
  id: "volatility-clustering",
  title: "Volatility Clustering",
  chapterId: "arma-models",
  content: `## Volatility Clustering

A well-known stylized fact in financial time series: **large price moves tend to cluster together**. Periods of high volatility are followed by more high volatility.

### Log Returns

For a price series \`prices\`, the log return at time \`t\` is:
\`\`\`
r[t] = ln(prices[t+1] / prices[t])
\`\`\`

### Squared Returns

Squared returns are a proxy for volatility:
\`\`\`
r[t]² 
\`\`\`

High squared return = high volatility at that point.

### Volatility Cluster Ratio

The fraction of returns with squared return exceeding a threshold:
\`\`\`
cluster_ratio = count(r[t]² > threshold) / n
\`\`\`

### Task

Implement:
- \`squared_returns(prices)\` → list of squared log returns
- \`volatility_cluster_ratio(prices, threshold)\` → fraction of high-volatility observations
`,
  starterCode: `import math

def squared_returns(prices):
    # Compute log returns then square them
    pass

def volatility_cluster_ratio(prices, threshold):
    # Fraction of squared returns above threshold
    pass
`,
  solution: `import math

def squared_returns(prices):
    log_rets = [math.log(prices[i+1] / prices[i]) for i in range(len(prices)-1)]
    return [r**2 for r in log_rets]

def volatility_cluster_ratio(prices, threshold):
    sq = squared_returns(prices)
    high_vol = sum(1 for v in sq if v > threshold)
    return high_vol / len(sq)
`,
  tests: [
    {
      name: "squared_returns for price series",
      code: `{{FUNC}}\nprices = [100.0, 102.0, 99.0, 105.0, 103.0, 108.0, 104.0, 110.0]\nprint([round(v, 6) for v in squared_returns(prices)])`,
      expected: "[0.000392, 0.000891, 0.003462, 0.00037, 0.002247, 0.001424, 0.003146]\n",
    },
    {
      name: "volatility_cluster_ratio above 0.0005",
      code: `{{FUNC}}\nprices = [100.0, 102.0, 99.0, 105.0, 103.0, 108.0, 104.0, 110.0]\nprint(round(volatility_cluster_ratio(prices, 0.0005), 4))`,
      expected: "0.7143\n",
    },
    {
      name: "squared_returns for smaller price series",
      code: `{{FUNC}}\nprices = [10.0, 11.0, 10.5, 12.0, 11.5, 13.0]\nprint([round(v, 6) for v in squared_returns(prices)])`,
      expected: "[0.009084, 0.002164, 0.017831, 0.001811, 0.015031]\n",
    },
    {
      name: "volatility_cluster_ratio all above threshold",
      code: `{{FUNC}}\nprices = [10.0, 11.0, 10.5, 12.0, 11.5, 13.0]\nprint(round(volatility_cluster_ratio(prices, 0.001), 4))`,
      expected: "1.0\n",
    },
  ],
};
