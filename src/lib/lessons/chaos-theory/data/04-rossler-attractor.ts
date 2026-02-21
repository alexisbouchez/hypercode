import type { Lesson } from "../../types";

export const rosslerAttractor: Lesson = {
  id: "rossler-attractor",
  title: "Rössler Attractor",
  chapterId: "classic-attractors",
  content: `## Rössler Attractor

The **Rössler system** (1976) is a set of three ordinary differential equations that produce one of the simplest examples of a strange attractor in 3D:

$$\\dot{x} = -y - z, \\quad \\dot{y} = x + a y, \\quad \\dot{z} = b + z(x - c)$$

With the classic parameters **a = 0.2, b = 0.2, c = 5.7**, the system exhibits chaotic behaviour. The attractor looks like a band that folds back on itself — the folding mechanism is a key intuition for how chaos arises in continuous systems.

Integration via the **Euler method** (x_{n+1} = x_n + dt · ẋ_n) is the simplest numerical approach. While not the most accurate, it captures the qualitative chaotic dynamics for small enough step sizes.

**Implement the following functions:**
- \`rossler_deriv(x, y, z, a, b, c)\` — compute the derivatives (ẋ, ẏ, ż) of the Rössler system
- \`rossler_euler(x0, y0, z0, a, b, c, dt, steps)\` — integrate using Euler's method for \`steps\` steps, returning the final (x, y, z)`,
  starterCode: `def rossler_deriv(x, y, z, a, b, c):
    # Return (dx, dy, dz) for the Rossler system
    pass

def rossler_euler(x0, y0, z0, a, b, c, dt, steps):
    # Euler-integrate the Rossler system, return final (x, y, z)
    pass`,
  solution: `def rossler_deriv(x, y, z, a, b, c):
    dx = -y - z
    dy = x + a * y
    dz = b + z * (x - c)
    return dx, dy, dz

def rossler_euler(x0, y0, z0, a, b, c, dt, steps):
    x, y, z = x0, y0, z0
    for _ in range(steps):
        dx, dy, dz = rossler_deriv(x, y, z, a, b, c)
        x += dt * dx
        y += dt * dy
        z += dt * dz
    return x, y, z`,
  tests: [
    {
      name: "rossler derivative at origin",
      code: `{{FUNC}}\ndx,dy,dz = rossler_deriv(0.0,0.0,0.0,0.2,0.2,5.7)\nprint(dx,dy,dz)`,
      expected: "-0.0 0.0 0.2\n",
    },
    {
      name: "zero steps returns initial",
      code: `{{FUNC}}\nx,y,z = rossler_euler(1.0,2.0,3.0,0.2,0.2,5.7,0.01,0)\nprint(x,y,z)`,
      expected: "1.0 2.0 3.0\n",
    },
    {
      name: "one euler step",
      code: `{{FUNC}}\nx,y,z = rossler_euler(1.0,0.0,0.0,0.2,0.2,5.7,0.1,1)\nprint(round(x,4),round(y,4),round(z,4))`,
      expected: "1.0 0.1 0.02\n",
    },
  ],
};
