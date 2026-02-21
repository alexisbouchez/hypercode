import type { Lesson } from "../../types";

export const archModelLesson: Lesson = {
  id: "arch-model",
  title: "ARCH Model",
  chapterId: "volatility-models",
  content: `## ARCH Model (Autoregressive Conditional Heteroskedasticity)

The **ARCH(1)** model, introduced by Robert Engle (1982), captures volatility clustering by modeling the conditional variance as a function of past squared returns:

\`\`\`
h[t] = ω + α · r[t-1]²
\`\`\`

where:
- \`h[t]\` = conditional variance at time t
- \`ω\` = long-run variance base (omega > 0)
- \`α\` = ARCH coefficient (0 < α < 1)
- \`r[t-1]\` = return at t-1
- \`h[0]\` = variance of the return series (sample variance)

The return at each step is: \`r[t] = ε[t] · √h[t]\` where \`ε[t] ~ N(0,1)\`.

### Task

Implement:
- \`arch_variance(returns, omega, alpha)\` → list of ARCH(1) conditional variances
- \`arch_simulate(n, omega, alpha, seed)\` → simulate \`n\` ARCH(1) returns using a seeded RNG
`,
  starterCode: `import math
import random

def arch_variance(returns, omega, alpha):
    # h[0] = variance(returns); h[t] = omega + alpha * returns[t-1]^2
    pass

def arch_simulate(n, omega, alpha, seed=42):
    # Use random.Random(seed) and .gauss(0,1)
    # r[t] = gauss(0,1) * sqrt(h[t])
    pass
`,
  solution: `import math
import random

def arch_variance(returns, omega, alpha):
    n = len(returns)
    mean_r = sum(returns) / n
    h0 = sum((r - mean_r)**2 for r in returns) / n
    h = [h0]
    for t in range(1, n):
        h.append(omega + alpha * returns[t-1]**2)
    return h

def arch_simulate(n, omega, alpha, seed=42):
    rng = random.Random(seed)
    h = [omega / (1 - alpha) if alpha < 1 else omega]
    returns = []
    for t in range(n):
        e = rng.gauss(0, 1)
        r = e * math.sqrt(h[-1])
        returns.append(r)
        h.append(omega + alpha * r**2)
    return returns
`,
  tests: [
    {
      name: "arch_variance initial variance",
      code: `{{FUNC}}\nreturns = [0.1, -0.2, 0.3, -0.1, 0.15]\nprint(round(arch_variance(returns, 0.01, 0.5)[0], 6))`,
      expected: "0.032\n",
    },
    {
      name: "arch_variance full sequence",
      code: `{{FUNC}}\nreturns = [0.1, -0.2, 0.3, -0.1, 0.15]\nprint([round(v, 6) for v in arch_variance(returns, 0.01, 0.5)])`,
      expected: "[0.032, 0.015, 0.03, 0.055, 0.015]\n",
    },
    {
      name: "arch_variance with omega=0.02 alpha=0.4",
      code: `{{FUNC}}\nreturns = [0.05, -0.1, 0.08, -0.03]\nprint([round(v, 6) for v in arch_variance(returns, 0.02, 0.4)])`,
      expected: "[0.00495, 0.021, 0.024, 0.02256]\n",
    },
    {
      name: "arch_simulate 5 returns with seed=42",
      code: `{{FUNC}}\nprint([round(v, 4) for v in arch_simulate(5, 0.01, 0.5, seed=42)])`,
      expected: "[-0.0204, -0.0175, -0.0112, 0.0704, -0.0143]\n",
    },
  ],
};
