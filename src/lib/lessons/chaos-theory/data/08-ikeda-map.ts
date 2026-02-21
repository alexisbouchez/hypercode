import type { Lesson } from "../../types";

export const ikedaMap: Lesson = {
  id: "ikeda-map",
  title: "Ikeda Map",
  chapterId: "classic-attractors",
  content: `## Ikeda Map

The **Ikeda map** models the dynamics of light in an optical cavity (a ring laser). It was introduced by Kensuke Ikeda in 1979 and was one of the first physical systems shown to exhibit optical chaos.

### The Map

\`\`\`
t_n   = 0.4 - 6 / (1 + x_n² + y_n²)
x_{n+1} = 1 + u·(x_n·cos(t_n) - y_n·sin(t_n))
y_{n+1} = u·(x_n·sin(t_n) + y_n·cos(t_n))
\`\`\`

The parameter **u** controls the strength of the nonlinearity:
- **u < ~0.6**: Fixed point attractor
- **u ≈ 0.7**: Period-2 cycle
- **u ≈ 0.9**: **Strange attractor** — chaotic dynamics

### Physical Interpretation

- (x, y) represents the electric field amplitude in the cavity
- t_n is a phase shift that depends on light intensity (x² + y²)
- The rotation by angle t_n followed by scaling by u models the round-trip

### Your Task

Implement:
1. \`ikeda_step(x, y, u)\` — compute one step of the Ikeda map
2. \`ikeda_iterate(x0, y0, u, n)\` — iterate n times, return final point`,
  starterCode: `import math

def ikeda_step(x, y, u):
    # Compute t = 0.4 - 6.0 / (1 + x^2 + y^2)
    # x_new = 1 + u*(x*cos(t) - y*sin(t))
    # y_new = u*(x*sin(t) + y*cos(t))
    pass

def ikeda_iterate(x0, y0, u, n):
    # Iterate n times from (x0, y0), return final (x, y)
    pass`,
  solution: `import math

def ikeda_step(x, y, u):
    t = 0.4 - 6.0 / (1.0 + x*x + y*y)
    x_new = 1.0 + u * (x * math.cos(t) - y * math.sin(t))
    y_new = u * (x * math.sin(t) + y * math.cos(t))
    return x_new, y_new

def ikeda_iterate(x0, y0, u, n):
    x, y = x0, y0
    for _ in range(n):
        x, y = ikeda_step(x, y, u)
    return x, y`,
  tests: [
    {
      name: "ikeda step at origin",
      code: `{{FUNC}}\nx,y = ikeda_step(0.0,0.0,0.9)\nprint(round(x,4),round(y,4))`,
      expected: "1.0 0.0\n",
    },
    {
      name: "zero iterations",
      code: `{{FUNC}}\nx,y = ikeda_iterate(1.0,1.0,0.9,0)\nprint(x,y)`,
      expected: "1.0 1.0\n",
    },
    {
      name: "ikeda step from (1,0)",
      code: `{{FUNC}}\nx,y = ikeda_step(1.0,0.0,0.9)\nprint(round(x,4),round(y,4))`,
      expected: "0.2288 -0.464\n",
    },
  ],
};
