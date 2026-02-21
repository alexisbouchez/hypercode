import type { Lesson } from "../../types";

export const oneForm: Lesson = {
  id: "one-form",
  title: "One-Form Fields",
  chapterId: "vector-fields",
  content: `# One-Form Fields

A **one-form field** (or covector field) assigns to each point a linear functional on tangent vectors. While a vector field eats a function and produces a number at each point, a one-form field eats a vector field and produces a scalar function.

The most natural one-form is the **differential** $df$ of a scalar function $f$:

$$df(V)(p) = V[f](p) = \\sum_i V^i(p)\\frac{\\partial f}{\\partial x^i}(p)$$

This is the **directional derivative** of $f$ along $V$ — a purely functional definition.

## Coordinate One-Forms

In coordinates $(x, y)$, the **coordinate one-forms** are $dx$ and $dy$:

$$dx(V) = V^x = \\text{the }x\\text{-component of }V$$
$$dy(V) = V^y = \\text{the }y\\text{-component of }V$$

These are the duals of the coordinate basis vectors $\\partial/\\partial x$ and $\\partial/\\partial y$.

## Example

For $f(x,y) = x^2 + y^2$:

$$df = 2x\\,dx + 2y\\,dy$$

Acting on the constant vector field $V = \\partial/\\partial x$ at the point $(3,4)$:

$$df(\\partial/\\partial x)(3,4) = 2 \\cdot 3 = 6$$

## Your Task

Implement \`df(f)\` that returns the differential one-form of a scalar function $f$. The one-form should be callable as \`df(f)(V)(*point)\` where \`V(point)\` returns the vector components.
`,
  starterCode: `def partial(i, f, h=1e-7):
    def dfn(*args):
        args = list(args)
        p = args[:]
        m = args[:]
        p[i] += h
        m[i] -= h
        return (f(*p) - f(*m)) / (2 * h)
    return dfn

def df(f):
    """Return the differential one-form of f.
    df(f)(V)(*point) = V[f] at point,
    where V is a function: point -> [V^0, V^1, ...].
    """
    pass

# f(x, y) = x^2 + y^2
f = lambda x, y: x**2 + y**2

V_x = lambda p: [1, 0]   # ∂/∂x
V_y = lambda p: [0, 1]   # ∂/∂y

print(f"{df(f)(V_x)(3.0, 4.0):.4f}")  # ∂f/∂x = 2x = 6.0
print(f"{df(f)(V_y)(3.0, 4.0):.4f}")  # ∂f/∂y = 2y = 8.0
`,
  solution: `def partial(i, f, h=1e-7):
    def dfn(*args):
        args = list(args)
        p = args[:]
        m = args[:]
        p[i] += h
        m[i] -= h
        return (f(*p) - f(*m)) / (2 * h)
    return dfn

def df(f):
    def omega(V):
        def at(*point):
            comps = V(list(point))
            n = len(comps)
            return sum(comps[i] * partial(i, f)(*point) for i in range(n))
        return at
    return omega

f = lambda x, y: x**2 + y**2

V_x = lambda p: [1, 0]
V_y = lambda p: [0, 1]

print(f"{df(f)(V_x)(3.0, 4.0):.4f}")
print(f"{df(f)(V_y)(3.0, 4.0):.4f}")
`,
  tests: [
    {
      name: "d(x²+y²)(∂/∂x) at (3,4) = 6",
      code: `{{FUNC}}
f = lambda x, y: x**2 + y**2
print(f"{df(f)(lambda p: [1, 0])(3.0, 4.0):.4f}")`,
      expected: "6.0000\n",
    },
    {
      name: "d(x²+y²)(∂/∂y) at (3,4) = 8",
      code: `{{FUNC}}
f = lambda x, y: x**2 + y**2
print(f"{df(f)(lambda p: [0, 1])(3.0, 4.0):.4f}")`,
      expected: "8.0000\n",
    },
    {
      name: "d(xy)(∂/∂x) at (2,3) = 3",
      code: `{{FUNC}}
g = lambda x, y: x * y
print(f"{df(g)(lambda p: [1, 0])(2.0, 3.0):.4f}")`,
      expected: "3.0000\n",
    },
    {
      name: "d(xy)(∂/∂y) at (2,3) = 2",
      code: `{{FUNC}}
g = lambda x, y: x * y
print(f"{df(g)(lambda p: [0, 1])(2.0, 3.0):.4f}")`,
      expected: "2.0000\n",
    },
  ],
};
