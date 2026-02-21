import type { Lesson } from "../../types";

export const mcVar: Lesson = {
  id: "mc-var",
  title: "Monte Carlo VaR",
  chapterId: "market-risk",
  content: `## Monte Carlo VaR

**Monte Carlo simulation** estimates VaR by generating thousands of synthetic return scenarios from an assumed distribution, then taking the empirical percentile. It is more flexible than the parametric approach and can handle non-linear instruments.

### Box-Muller Transform

To generate standard normal samples from uniform randoms \`U1, U2 ∈ (0,1)\`:

\`\`\`
Z1 = sqrt(-2 ln U1) · cos(2π U2)
Z2 = sqrt(-2 ln U1) · sin(2π U2)
\`\`\`

Each pair \`(Z1, Z2)\` gives two independent standard normals.

### Algorithm

1. Set random seed for reproducibility
2. Generate \`n_sim\` returns: \`r = μ + σ · Z\`
3. Sort simulated returns
4. VaR = −return at index \`int((1 − confidence) × n_sim)\`

### Example

μ = 0.001, σ = 0.02, n = 10000, seed = 42, 95% confidence → **VaR ≈ 0.0316**
`,
  starterCode: `import math
import random

def mc_var(mu, sigma, n_sim, confidence=0.95, seed=42):
    pass
`,
  solution: `import math
import random

def mc_var(mu, sigma, n_sim, confidence=0.95, seed=42):
    random.seed(seed)
    returns = []
    for _ in range(n_sim // 2):
        u1 = random.random()
        u2 = random.random()
        z1 = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
        z2 = math.sqrt(-2 * math.log(u1)) * math.sin(2 * math.pi * u2)
        returns.append(mu + sigma * z1)
        returns.append(mu + sigma * z2)
    returns.sort()
    idx = int((1 - confidence) * len(returns))
    return -returns[idx]
`,
  tests: [
    {
      name: "mc_var mu=0.001 sigma=0.02 n=10000 seed=42 at 95%",
      code: `{{FUNC}}\nprint(round(mc_var(0.001, 0.02, 10000, 0.95, 42), 4))`,
      expected: "0.0316\n",
    },
    {
      name: "mc_var mu=0.0 sigma=0.01 n=10000 seed=42 at 99%",
      code: `{{FUNC}}\nprint(round(mc_var(0.0, 0.01, 10000, 0.99, 42), 4))`,
      expected: "0.0231\n",
    },
    {
      name: "mc_var mu=0.002 sigma=0.03 n=5000 seed=123 at 95%",
      code: `{{FUNC}}\nprint(round(mc_var(0.002, 0.03, 5000, 0.95, 123), 4))`,
      expected: "0.0494\n",
    },
    {
      name: "mc_var is deterministic with fixed seed",
      code: `{{FUNC}}\nr1 = mc_var(0.001, 0.02, 1000, 0.95, 7)\nr2 = mc_var(0.001, 0.02, 1000, 0.95, 7)\nprint(r1 == r2)`,
      expected: "True\n",
    },
  ],
};
