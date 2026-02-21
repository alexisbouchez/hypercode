import type { Lesson } from "../../types";

export const lyapunovSpectrum: Lesson = {
  id: "lyapunov-spectrum",
  title: "Lyapunov Exponents",
  chapterId: "lyapunov",
  content: `## Lyapunov Exponents

The **Lyapunov exponent** λ quantifies how quickly nearby trajectories diverge (or converge). For a 1D map f, it is computed by averaging the log of the absolute derivative along the orbit:

$$\\lambda = \\frac{1}{n} \\sum_{i=0}^{n-1} \\ln |f'(x_i)|$$

For the **logistic map** f(x) = r·x·(1−x), the derivative is f'(x) = r·(1−2x). By computing λ numerically, you can determine whether the system is in a stable, periodic, or fully chaotic regime.

A **positive Lyapunov exponent** means chaos: infinitesimally close initial conditions separate exponentially fast, making long-term prediction impossible. At r = 4, the logistic map is fully chaotic with λ = ln(2) ≈ 0.693. Note: when the derivative is exactly zero (as at a fixed point), skip that term to avoid a domain error.

**Implement the following functions:**
- \`logistic_lyapunov(r, x0, n)\` — compute the Lyapunov exponent for the logistic map at parameter \`r\`, starting from \`x0\`, using \`n\` iterations (skip terms where |f'(x)| ≤ 1e-10)
- \`is_chaotic(lam)\` — return \`True\` if the exponent indicates chaos (λ > 0)`,
  starterCode: `import math

def logistic_lyapunov(r, x0, n):
    # Compute Lyapunov exponent: average of log|f'(x_i)|
    # f'(x) = r * (1 - 2*x); skip if |f'| <= 1e-10
    pass

def is_chaotic(lam):
    # Return True if lam > 0
    pass`,
  solution: `import math

def logistic_lyapunov(r, x0, n):
    x = x0
    total = 0.0
    count = 0
    for _ in range(n):
        deriv = r * (1.0 - 2.0 * x)
        if abs(deriv) > 1e-10:
            total += math.log(abs(deriv))
            count += 1
        x = r * x * (1.0 - x)
    return total / count if count > 0 else float('-inf')

def is_chaotic(lam):
    return lam > 0.0`,
  tests: [
    {
      name: "r=2 is stable",
      code: `{{FUNC}}\nlam = logistic_lyapunov(2.0, 0.1, 2000)\nprint(is_chaotic(lam))`,
      expected: "False\n",
    },
    {
      name: "r=4 is chaotic",
      code: `{{FUNC}}\nlam = logistic_lyapunov(4.0, 0.1, 2000)\nprint(is_chaotic(lam))`,
      expected: "True\n",
    },
    {
      name: "r=4 lyapunov near ln2",
      code: `{{FUNC}}\nlam = logistic_lyapunov(4.0, 0.1, 10000)\nprint(round(lam, 2))`,
      expected: "0.69\n",
    },
  ],
};
