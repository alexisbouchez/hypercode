import type { Lesson } from "../../types";

export const monteCarlo: Lesson = {
  id: "monte-carlo",
  title: "Monte Carlo Integration",
  chapterId: "numerical",
  content: `# Monte Carlo Integration

**Monte Carlo integration** uses random sampling to estimate integrals — particularly powerful in high dimensions where deterministic quadrature becomes infeasible.

## Basic Idea

To compute $\\int_a^b f(x)\\, dx$, draw $N$ uniform random samples $x_i \\in [a, b]$ and estimate:

$$\\int_a^b f(x)\\, dx \\approx \\frac{b-a}{N} \\sum_{i=1}^N f(x_i)$$

The **error** scales as $1/\\sqrt{N}$ regardless of dimension — a huge advantage over grid-based methods in $d$ dimensions (which scale as $1/N^{2/d}$).

## Importance Sampling

When $f$ is sharply peaked, plain sampling is inefficient. Instead, sample from a distribution $p(x)$ that is large where $f$ is large:

$$\\int f(x)\\, dx = \\int \\frac{f(x)}{p(x)}\\, p(x)\\, dx \\approx \\frac{1}{N} \\sum_{i=1}^N \\frac{f(x_i)}{p(x_i)}$$

Optimal choice: $p(x) \\propto |f(x)|$ (reduces variance to zero for positive $f$).

## Estimating π

A classic application: generate random points $(x, y) \\in [0,1)^2$. The fraction landing inside the unit quarter-circle ($x^2 + y^2 < 1$) converges to $\\pi/4$:

$$\\pi \\approx \\frac{4 \\cdot \\text{(points inside circle)}}{N}$$

## Reproducible Randomness: LCG

For reproducibility, we use a **Linear Congruential Generator (LCG)**:

$$s_{n+1} = (a \\cdot s_n + c) \\bmod m, \\qquad x_n = s_n / m$$

with parameters $a = 1{,}664{,}525$, $c = 1{,}013{,}904{,}223$, $m = 2^{32}$ (Numerical Recipes constants). Starting from a seed $s_0$, this generates a deterministic sequence of uniform pseudo-random numbers in $[0, 1)$.

## Implementation

- \`lcg_random(seed, n)\` — returns list of $n$ uniform $[0,1)$ values via LCG
- \`monte_carlo_integrate(f_func, a, b, N=10000, seed=42)\` — plain Monte Carlo estimate
- \`monte_carlo_pi(N=10000, seed=42)\` — estimate $\\pi$ using the unit circle method

\`\`\`python
import math

def lcg_random(seed, n):
    a = 1664525
    c = 1013904223
    m = 2**32
    state = seed
    result = []
    for _ in range(n):
        state = (a * state + c) % m
        result.append(state / m)
    return result
\`\`\`
`,
  starterCode: `import math

def lcg_random(seed, n):
    pass

def monte_carlo_integrate(f_func, a, b, N=10000, seed=42):
    pass

def monte_carlo_pi(N=10000, seed=42):
    pass
`,
  solution: `import math

def lcg_random(seed, n):
    a = 1664525
    c = 1013904223
    m = 2**32
    state = seed
    result = []
    for _ in range(n):
        state = (a * state + c) % m
        result.append(state / m)
    return result

def monte_carlo_integrate(f_func, a, b, N=10000, seed=42):
    xs = lcg_random(seed, N)
    total = 0.0
    for x_unit in xs:
        x = a + (b - a) * x_unit
        total += f_func(x)
    return (b - a) * total / N

def monte_carlo_pi(N=10000, seed=42):
    vals = lcg_random(seed, 2 * N)
    count = 0
    for i in range(N):
        x = vals[2 * i]
        y = vals[2 * i + 1]
        if x * x + y * y < 1.0:
            count += 1
    return 4.0 * count / N
`,
  tests: [
    {
      name: "lcg_random(42, 5)[0] first value from seed=42",
      expected: "0.252345\n",
      code: `{{FUNC}}\nprint(f"{lcg_random(42, 5)[0]:.6f}")`,
    },
    {
      name: "monte_carlo_integrate(x^2, 0, 1, N=100000) ≈ 1/3",
      expected: "0.3343\n",
      code: `{{FUNC}}\nprint(f"{monte_carlo_integrate(lambda x: x**2, 0, 1, N=100000, seed=42):.4f}")`,
    },
    {
      name: "monte_carlo_integrate(sin(x), 0, pi, N=100000) ≈ 2.0",
      expected: "2.0038\n",
      code: `{{FUNC}}\nprint(f"{monte_carlo_integrate(lambda x: math.sin(x), 0, math.pi, N=100000, seed=42):.4f}")`,
    },
    {
      name: "monte_carlo_pi(N=100000) ≈ pi",
      expected: "3.1500\n",
      code: `{{FUNC}}\nprint(f"{monte_carlo_pi(N=100000, seed=42):.4f}")`,
    },
  ],
};
