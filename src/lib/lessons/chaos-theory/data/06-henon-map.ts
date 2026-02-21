import type { Lesson } from "../../types";

export const henonMap: Lesson = {
  id: "henon-map",
  title: "Hénon Map",
  chapterId: "classic-attractors",
  content: `## Hénon Map

The **Hénon map** is a classic 2D discrete dynamical system that exhibits a strange attractor. It was introduced by Michel Hénon in 1976 as a simplified model of the Poincaré section of the Lorenz attractor.

### The Map

Given a point (x, y), the next point is:

\`\`\`
x_{n+1} = 1 - a·x_n² + y_n
y_{n+1} = b·x_n
\`\`\`

With parameters **a = 1.4** and **b = 0.3**, the map produces a **strange attractor** — a fractal structure with dimension ≈ 1.26.

### Properties

- **Area contraction**: The Jacobian determinant is \`-b\`, so volumes shrink by factor \`|b|\` = 0.3 each step
- **Strange attractor**: For a=1.4, b=0.3, almost all initial conditions converge to the attractor
- **Fractal structure**: The attractor has a self-similar, layered structure

### Implement the Hénon Map

Implement three functions:
1. \`henon_step(x, y, a, b)\` — compute one iteration
2. \`henon_iterate(x0, y0, a, b, n)\` — iterate n times and return final point
3. \`henon_orbit(x0, y0, a, b, n)\` — return list of x-coordinates for n iterations`,
  starterCode: `def henon_step(x, y, a, b):
    # Compute one step of the Hénon map
    # x_new = 1 - a*x^2 + y
    # y_new = b*x
    pass

def henon_iterate(x0, y0, a, b, n):
    # Iterate the map n times from (x0, y0)
    # Return final (x, y)
    pass

def henon_orbit(x0, y0, a, b, n):
    # Return list of x values (rounded to 6 decimals) after each step
    pass`,
  solution: `def henon_step(x, y, a, b):
    x_new = 1.0 - a * x * x + y
    y_new = b * x
    return x_new, y_new

def henon_iterate(x0, y0, a, b, n):
    x, y = x0, y0
    for _ in range(n):
        x, y = henon_step(x, y, a, b)
    return x, y

def henon_orbit(x0, y0, a, b, n):
    xs = []
    x, y = x0, y0
    for _ in range(n):
        x, y = henon_step(x, y, a, b)
        xs.append(round(x, 6))
    return xs`,
  tests: [
    {
      name: "henon step at origin",
      code: `{{FUNC}}\nprint(henon_step(0.0, 0.0, 1.4, 0.3))`,
      expected: "(1.0, 0.0)\n",
    },
    {
      name: "henon step from (1,0)",
      code: `{{FUNC}}\nx, y = henon_step(1.0, 0.0, 1.4, 0.3)\nprint(round(x, 4), round(y, 4))`,
      expected: "-0.4 0.3\n",
    },
    {
      name: "zero iterations",
      code: `{{FUNC}}\nprint(henon_iterate(0.5, 0.5, 1.4, 0.3, 0))`,
      expected: "(0.5, 0.5)\n",
    },
  ],
};
