import type { Lesson } from "../../types";

export const logisticMap: Lesson = {
  id: "logistic-map",
  title: "Logistic Map",
  chapterId: "dynamical-systems",
  content: `# Logistic Map

The **logistic map** is one of the simplest yet most fascinating examples of how complex, chaotic behaviour can emerge from a simple nonlinear equation:

$$x_{n+1} = r \\cdot x_n \\cdot (1 - x_n)$$

Here $x_n \\in [0,1]$ represents a normalised population at generation $n$, and $r$ is the growth rate parameter.

## Behaviour by Region

| Range of $r$ | Behaviour |
|---|---|
| $r < 1$ | Population dies out → $x^* = 0$ |
| $1 < r < 3$ | Converges to stable fixed point $x^* = 1 - 1/r$ |
| $3 < r < 3.57$ | Period-doubling bifurcations (2, 4, 8, …) |
| $r > 3.57$ | Chaos (mostly) |

## Fixed Point

For $r > 1$, the non-trivial fixed point satisfies $x^* = r x^* (1 - x^*)$, giving:

$$x^* = 1 - \\frac{1}{r}$$

## Lyapunov Exponent

The **Lyapunov exponent** $\\lambda$ measures the average rate of divergence of nearby trajectories:

$$\\lambda = \\frac{1}{N} \\sum_{n=0}^{N-1} \\ln \\left| r(1 - 2x_n) \\right|$$

- $\\lambda < 0$: stable (convergent) dynamics
- $\\lambda > 0$: chaos (exponential divergence)

For the fully chaotic case $r = 4$, the Lyapunov exponent equals $\\ln 2 \\approx 0.693$.

## Your Task

Implement three functions:
- \`logistic_iterate(r, x0, n)\` — iterate the map $n$ times from $x_0$, returning a list of $n$ values
- \`logistic_fixed_point(r)\` — return $x^* = 1 - 1/r$ for $r > 1$, else $0$
- \`lyapunov_exponent(r, x0=0.5, n=1000)\` — compute $\\lambda$, skipping the first 100 transient steps
`,
  starterCode: `import math

def logistic_iterate(r, x0, n):
    pass

def logistic_fixed_point(r):
    pass

def lyapunov_exponent(r, x0=0.5, n=1000):
    pass
`,
  solution: `import math

def logistic_iterate(r, x0, n):
    vals = []
    x = x0
    for _ in range(n):
        x = r * x * (1 - x)
        vals.append(x)
    return vals

def logistic_fixed_point(r):
    if r <= 1:
        return 0.0
    return 1.0 - 1.0 / r

def lyapunov_exponent(r, x0=0.5, n=1000):
    x = x0
    for _ in range(100):
        x = r * x * (1 - x)
    total = 0.0
    for _ in range(n - 100):
        total += math.log(abs(r * (1 - 2 * x)))
        x = r * x * (1 - x)
    return total / (n - 100)
`,
  tests: [
    {
      name: "logistic_fixed_point(2.5) returns 0.6",
      expected: "0.6000\n",
      code: `{{FUNC}}
print(f"{logistic_fixed_point(2.5):.4f}")`,
    },
    {
      name: "logistic_iterate(2.5, 0.5, 1000) converges to fixed point",
      expected: "0.6000\n",
      code: `{{FUNC}}
print(f"{logistic_iterate(2.5, 0.5, 1000)[-1]:.4f}")`,
    },
    {
      name: "lyapunov_exponent(2.5) is negative (stable)",
      expected: "-0.6931\n",
      code: `{{FUNC}}
print(f"{lyapunov_exponent(2.5):.4f}")`,
    },
    {
      name: "lyapunov_exponent(4.0) is positive (chaotic)",
      expected: "1.3863\n",
      code: `{{FUNC}}
print(f"{lyapunov_exponent(4.0):.4f}")`,
    },
  ],
};
