import type { Lesson } from "../../types";

export const tangentVector: Lesson = {
  id: "tangent-vector",
  title: "Tangent Vectors as Derivations",
  chapterId: "manifolds",
  content: `# Tangent Vectors as Derivations

Traditional calculus defines a tangent vector as an arrow at a point. But this geometric picture breaks down on abstract manifolds. The book takes the **algebraic** approach: a tangent vector is defined by how it acts on functions.

A **tangent vector** $v$ at a point $p$ is a **derivation** — a linear map from smooth functions to real numbers:

$$v[f] = \\sum_i v^i \\frac{\\partial f}{\\partial x^i}(p)$$

where $v^0, v^1, \\ldots$ are the **components** of $v$ in the coordinate basis.

## The Coordinate Basis

For rectangular coordinates $(x, y)$, the coordinate basis vectors are:
$$\\partial/\\partial x: \\quad f \\mapsto \\frac{\\partial f}{\\partial x}(p)$$
$$\\partial/\\partial y: \\quad f \\mapsto \\frac{\\partial f}{\\partial y}(p)$$

These are the "unit vectors" in each coordinate direction, expressed as operators on functions.

## Example

The vector $v = [3, -1]$ (meaning $3\\partial/\\partial x - \\partial/\\partial y$) acts on $f(x,y) = x^2 + y^2$:

$$v[f](1, 2) = 3 \\cdot \\frac{\\partial}{\\partial x}(x^2+y^2)\\Big|_{(1,2)} + (-1) \\cdot \\frac{\\partial}{\\partial y}(x^2+y^2)\\Big|_{(1,2)}$$
$$= 3 \\cdot 2 + (-1) \\cdot 4 = 2$$

## Your Task

Implement \`tangent_vector(components)\` that returns a function \`v\` such that \`v(f)(*point)\` computes the directional derivative of \`f\` in direction \`components\` at \`point\`.
`,
  starterCode: `def partial(i, f, h=1e-7):
    def df(*args):
        args = list(args)
        p = args[:]
        m = args[:]
        p[i] += h
        m[i] -= h
        return (f(*p) - f(*m)) / (2 * h)
    return df

def tangent_vector(components):
    """Return v such that v(f)(*point) = sum_i components[i] * ∂f/∂x_i at point."""
    pass

# ∂/∂x acting on f(x,y) = x^2 at (2, 3)
d_dx = tangent_vector([1, 0])
f = lambda x, y: x**2
print(f"{d_dx(f)(2.0, 3.0):.4f}")  # 2x = 4.0000

# General vector [2, -1] acting on g(x,y) = x*y at (3, 4)
v = tangent_vector([2, -1])
g = lambda x, y: x * y
print(f"{v(g)(3.0, 4.0):.4f}")   # 2*y - 1*x = 2*4 - 3 = 5.0000
`,
  solution: `def partial(i, f, h=1e-7):
    def df(*args):
        args = list(args)
        p = args[:]
        m = args[:]
        p[i] += h
        m[i] -= h
        return (f(*p) - f(*m)) / (2 * h)
    return df

def tangent_vector(components):
    def v(f):
        def action(*point):
            n = len(components)
            return sum(components[i] * partial(i, f)(*point) for i in range(n))
        return action
    return v

d_dx = tangent_vector([1, 0])
f = lambda x, y: x**2
print(f"{d_dx(f)(2.0, 3.0):.4f}")

v = tangent_vector([2, -1])
g = lambda x, y: x * y
print(f"{v(g)(3.0, 4.0):.4f}")
`,
  tests: [
    {
      name: "[1,0](x²) at (2,3) = 4",
      code: `{{FUNC}}
d_dx = tangent_vector([1, 0])
print(f"{d_dx(lambda x, y: x**2)(2.0, 3.0):.4f}")`,
      expected: "4.0000\n",
    },
    {
      name: "[0,1](y²) at (2,3) = 6",
      code: `{{FUNC}}
d_dy = tangent_vector([0, 1])
print(f"{d_dy(lambda x, y: y**2)(2.0, 3.0):.4f}")`,
      expected: "6.0000\n",
    },
    {
      name: "[1,1](x+y) at (1,1) = 2",
      code: `{{FUNC}}
v = tangent_vector([1, 1])
print(f"{v(lambda x, y: x + y)(1.0, 1.0):.4f}")`,
      expected: "2.0000\n",
    },
    {
      name: "[2,-1](xy) at (3,4) = 5",
      code: `{{FUNC}}
v = tangent_vector([2, -1])
print(f"{v(lambda x, y: x * y)(3.0, 4.0):.4f}")`,
      expected: "5.0000\n",
    },
  ],
};
