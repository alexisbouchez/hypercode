import type { Lesson } from "../../types";

export const monteCarlo: Lesson = {
  id: "monte-carlo",
  title: "Monte Carlo Sampling",
  chapterId: "regression",
  content: `## Monte Carlo Sampling

**Monte Carlo simulation** uses random sampling to model uncertainty. In quantitative finance, it is used to price derivatives, estimate risk (VaR), and simulate portfolio paths.

A key building block is generating samples from a **normal distribution** using the **Box-Muller transform**. Given two independent uniform samples $U_1, U_2 \\in (0, 1)$:

$$Z_0 = \\sqrt{-2 \\ln U_1} \\cos(2\\pi U_2)$$
$$Z_1 = \\sqrt{-2 \\ln U_1} \\sin(2\\pi U_2)$$

Both $Z_0$ and $Z_1$ are independent standard normal samples. Scale to mean $\\mu$ and std $\\sigma$: $X = \\mu + \\sigma Z$.

This transform is efficient because each pair $(U_1, U_2)$ yields two normal samples.

### Your Task

Implement:
- \`mc_normal_samples(n, mu, sigma, seed)\` — generate \`n\` normal samples using Box-Muller and \`random.seed(seed)\`
- \`mc_mean(samples)\` — mean of a list of samples`,
  starterCode: `import math
import random

def mc_normal_samples(n, mu, sigma, seed):
    # Use Box-Muller: generate pairs (Z0, Z1) from uniform samples
    # random.seed(seed) before generating
    pass

def mc_mean(samples):
    # mean of samples
    pass`,
  solution: `import math
import random

def mc_normal_samples(n, mu, sigma, seed):
    random.seed(seed)
    samples = []
    while len(samples) < n:
        u1 = random.random()
        u2 = random.random()
        if u1 == 0:
            continue
        z0 = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
        z1 = math.sqrt(-2 * math.log(u1)) * math.sin(2 * math.pi * u2)
        samples.append(mu + sigma * z0)
        if len(samples) < n:
            samples.append(mu + sigma * z1)
    return samples[:n]

def mc_mean(samples):
    return sum(samples) / len(samples)`,
  tests: [
    {
      name: "mc_mean of 1000 N(0,1) samples (seed=42) is near 0",
      code: `{{FUNC}}
samples = mc_normal_samples(1000, 0, 1, 42)
print(round(mc_mean(samples), 4))`,
      expected: "-0.0074\n",
    },
    {
      name: "mc_mean of 500 N(5,2) samples (seed=7) is near 5",
      code: `{{FUNC}}
samples = mc_normal_samples(500, 5, 2, 7)
print(round(mc_mean(samples), 4))`,
      expected: "5.0816\n",
    },
    {
      name: "mc_normal_samples returns exactly n samples",
      code: `{{FUNC}}
samples = mc_normal_samples(100, 0, 1, 1)
print(len(samples))`,
      expected: "100\n",
    },
  ],
};
