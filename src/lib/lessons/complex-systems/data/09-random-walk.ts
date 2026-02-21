import type { Lesson } from "../../types";

export const randomWalk: Lesson = {
  id: "random-walk",
  title: "Random Walks",
  chapterId: "statistical-physics",
  content: `# Random Walks

A **random walk** is one of the most fundamental stochastic processes in science, underpinning Brownian motion, diffusion, polymer conformations, financial models, and search algorithms.

## 1D Random Walk

In the simplest version, a walker on the integer number line takes steps of $+1$ or $-1$ with equal probability. After $N$ steps the walker is at position $x_N = \\sum_{i=1}^N \\xi_i$ where each $\\xi_i \\in \\{+1, -1\\}$.

Key results:
- **Mean displacement**: $\\langle x_N \\rangle = 0$ (symmetric walk)
- **Mean squared displacement (MSD)**: $\\langle x_N^2 \\rangle = N$
- **Root-mean-square displacement**: $\\sqrt{\\langle x_N^2 \\rangle} = \\sqrt{N}$

The MSD grows linearly with time — this is the signature of **diffusion**. In general, for diffusion coefficient $D$:

$$\\langle x^2(t) \\rangle = 2 D t$$

(in 1D, where $t$ counts discrete steps and $D = 1/2$ for unit steps of size 1).

## Reproducibility: Linear Congruential Generator

For deterministic, reproducible walks we use a **Linear Congruential Generator (LCG)**:

$$s_{n+1} = (a \\cdot s_n + c) \\bmod m$$

with parameters $a = 1{,}664{,}525$, $c = 1{,}013{,}904{,}223$, $m = 2^{32}$.

A uniform value in $[0,1)$ is obtained as $u = s_{n+1} / m$. If $u < 0.5$ the walker steps $+1$, otherwise $-1$.

## Mean First Passage Time

For a 1D symmetric random walk starting at the origin, the expected number of steps to first reach position $\\pm L$ (the **mean first passage time**) is:

$$\\langle T \\rangle = L^2$$

This quadratic scaling has practical consequences: diffusion is a slow search strategy for large distances.

## Your Task

Implement four functions:
- \`lcg_step(state)\` — one LCG step, returning \`(value, new_state)\` where \`value\` is in $[0,1)$
- \`random_walk_1d(N, seed=42)\` — simulate a 1D random walk of $N$ steps using the LCG, return final position
- \`random_walk_variance(N)\` — return the theoretical variance $\\langle x_N^2 \\rangle = N$
- \`msd_theoretical(N, D=1)\` — return the theoretical MSD $= 2 D N$ for 1D diffusion
`,
  starterCode: `def lcg_step(state):
    pass

def random_walk_1d(N, seed=42):
    pass

def random_walk_variance(N):
    pass

def msd_theoretical(N, D=1):
    pass
`,
  solution: `def lcg_step(state):
    a = 1664525
    c = 1013904223
    m = 2 ** 32
    new_state = (a * state + c) % m
    value = new_state / m
    return (value, new_state)

def random_walk_1d(N, seed=42):
    state = seed
    pos = 0
    for _ in range(N):
        value, state = lcg_step(state)
        if value < 0.5:
            pos += 1
        else:
            pos -= 1
    return pos

def random_walk_variance(N):
    return N

def msd_theoretical(N, D=1):
    return 2 * D * N
`,
  tests: [
    {
      name: "random_walk_variance(100) equals 100",
      expected: "100\n",
      code: `{{FUNC}}
print(random_walk_variance(100))`,
    },
    {
      name: "msd_theoretical(100) equals 200",
      expected: "200\n",
      code: `{{FUNC}}
print(msd_theoretical(100))`,
    },
    {
      name: "random_walk_1d(1000, seed=42) is deterministic",
      expected: "-26\n",
      code: `{{FUNC}}
print(random_walk_1d(1000, seed=42))`,
    },
    {
      name: "random_walk_1d(1000, seed=123) gives different result",
      expected: "-16\n",
      code: `{{FUNC}}
print(random_walk_1d(1000, seed=123))`,
    },
  ],
};
