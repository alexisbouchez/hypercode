import type { Lesson } from "../../types";

export const coordinates: Lesson = {
  id: "coordinates",
  title: "Coordinate Systems",
  chapterId: "manifolds",
  content: `# Coordinate Systems

A **manifold** is a space that locally looks like $\\mathbb{R}^n$. To do calculus on a manifold, we need **coordinate systems** — smooth maps that identify regions of the manifold with regions of $\\mathbb{R}^n$.

A coordinate system consists of two inverse functions:
- **chart** $\\chi$: maps manifold points → coordinate tuples (e.g., $(x,y)$ or $(r,\\theta)$)
- **point** $\\chi^{-1}$: maps coordinate tuples → manifold points

The book introduces these as \`(chart coordsys)\` and \`(point coordsys)\` in Scheme. In Python, we represent a coordinate system as a pair of functions.

## Example: Polar Coordinates on $\\mathbb{R}^2$

A point $p = (x, y)$ in rectangular coordinates has polar coordinates:

$$r = \\sqrt{x^2 + y^2}, \\quad \\theta = \\text{atan2}(y, x)$$

Going back from polar to rectangular:

$$x = r\\cos\\theta, \\quad y = r\\sin\\theta$$

## Coordinate Independence

The key insight: a **manifold point** is a geometric object independent of coordinates. The point $(3, 4)$ in rectangular and the point $(5, \\text{atan2}(4,3))$ in polar refer to the **same point** on the manifold. Both coordinate systems describe the same reality.

## Your Task

Implement the polar coordinate system:
- \`polar_chart(p)\`: convert rectangular point $(x, y)$ → polar coordinates $(r, \\theta)$
- \`polar_point(c)\`: convert polar coordinates $(r, \\theta)$ → rectangular point $(x, y)$
`,
  starterCode: `import math

def polar_chart(p):
    """Convert rectangular (x, y) to polar (r, theta)."""
    pass

def polar_point(c):
    """Convert polar (r, theta) to rectangular (x, y)."""
    pass

p = (3.0, 4.0)
c = polar_chart(p)
print(f"{c[0]:.4f}")   # r = 5.0000
print(f"{c[1]:.4f}")   # theta = 0.9273

# Round-trip: rect -> polar -> rect
p_back = polar_point(c)
print(f"{p_back[0]:.4f}")  # 3.0000
print(f"{p_back[1]:.4f}")  # 4.0000
`,
  solution: `import math

def polar_chart(p):
    x, y = p
    return (math.sqrt(x**2 + y**2), math.atan2(y, x))

def polar_point(c):
    r, theta = c
    return (r * math.cos(theta), r * math.sin(theta))

p = (3.0, 4.0)
c = polar_chart(p)
print(f"{c[0]:.4f}")
print(f"{c[1]:.4f}")

p_back = polar_point(c)
print(f"{p_back[0]:.4f}")
print(f"{p_back[1]:.4f}")
`,
  tests: [
    {
      name: "polar_chart((3,4))[0] = 5",
      code: `{{FUNC}}
print(f"{polar_chart((3.0, 4.0))[0]:.4f}")`,
      expected: "5.0000\n",
    },
    {
      name: "polar_chart((1,0))[1] = 0 (theta)",
      code: `{{FUNC}}
print(f"{polar_chart((1.0, 0.0))[1]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "polar_point((1, π/2))[0] ≈ 0 (x)",
      code: `{{FUNC}}
import math
print(f"{polar_point((1.0, math.pi/2))[0]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "polar_point((1, π/2))[1] = 1 (y)",
      code: `{{FUNC}}
import math
print(f"{polar_point((1.0, math.pi/2))[1]:.4f}")`,
      expected: "1.0000\n",
    },
  ],
};
