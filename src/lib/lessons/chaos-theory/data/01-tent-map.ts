import type { Lesson } from "../../types";

export const tentMap: Lesson = {
  id: "tent-map",
  title: "The Tent Map",
  chapterId: "lyapunov",
  content: `## The Tent Map

The **tent map** is one of the simplest chaotic dynamical systems. It is a piecewise-linear map defined on [0,1]:

$$f(x) = r \\cdot x \\quad \\text{if } x < 0.5, \\quad f(x) = r \\cdot (1 - x) \\quad \\text{otherwise}$$

For the parameter value **r = 2**, the tent map is exactly chaotic: almost every initial condition produces a trajectory that never settles into a periodic orbit. The map is a foundational example because its Lyapunov exponent — the average rate of exponential divergence between nearby trajectories — can be computed analytically.

Because |f'(x)| = r everywhere (except at x = 0.5), the **Lyapunov exponent** is simply ln(r). For r = 2, λ = ln(2) ≈ 0.693, confirming exponential sensitivity to initial conditions.

**Implement the following functions:**
- \`tent_map(x0, r, n)\` — iterate the tent map starting from \`x0\` for \`n\` steps, returning a list of \`n+1\` values (including the initial value)
- \`tent_lyapunov(r)\` — return the analytic Lyapunov exponent ln(r)`,
  starterCode: `import math

def tent_map(x0, r, n):
    # Return list of n+1 values: [x0, f(x0), f(f(x0)), ...]
    pass

def tent_lyapunov(r):
    # Lyapunov exponent of the tent map is log(r)
    pass`,
  solution: `import math

def tent_map(x0, r, n):
    xs = [x0]
    x = x0
    for _ in range(n):
        if x < 0.5:
            x = r * x
        else:
            x = r * (1.0 - x)
        xs.append(x)
    return xs

def tent_lyapunov(r):
    return math.log(r)`,
  tests: [
    {
      name: "tent_map iterates correctly",
      code: `{{FUNC}}\nprint([round(v,4) for v in tent_map(0.1, 2.0, 3)])`,
      expected: "[0.1, 0.2, 0.4, 0.8]\n",
    },
    {
      name: "tent_lyapunov for r=2",
      code: `{{FUNC}}\nprint(round(tent_lyapunov(2.0), 4))`,
      expected: "0.6931\n",
    },
    {
      name: "tent_map at boundary",
      code: `{{FUNC}}\nprint([round(v,4) for v in tent_map(0.6, 2.0, 3)])`,
      expected: "[0.6, 0.8, 0.4, 0.8]\n",
    },
  ],
};
