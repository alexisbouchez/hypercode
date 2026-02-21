import type { Lesson } from "../../types";

export const vectorField: Lesson = {
  id: "vector-field",
  title: "Vector Fields",
  chapterId: "vector-fields",
  content: `# Vector Fields

A **vector field** assigns a tangent vector to every point of a manifold. In coordinates, a vector field $V$ has components $V^i(p)$ that vary smoothly from point to point.

A vector field acts as a **differential operator** on scalar functions:

$$V[f](p) = \\sum_i V^i(p) \\frac{\\partial f}{\\partial x^i}(p)$$

This produces a new scalar function — the directional derivative of $f$ along $V$.

## Important Vector Fields on $\\mathbb{R}^2$

**Radial field**: $X = x\\partial/\\partial x + y\\partial/\\partial y$

This field points radially outward. Acting on $f = x^2 + y^2$:
$$X[f](1,2) = x \\cdot 2x + y \\cdot 2y = 2 + 8 = 10$$

**Rotation field**: $Y = -y\\partial/\\partial x + x\\partial/\\partial y$

This field rotates counterclockwise. Acting on $f = x^2 + y^2$ (which is rotationally symmetric):
$$Y[f](1,2) = -y \\cdot 2x + x \\cdot 2y = -4 + 4 = 0$$

The rotation field kills radially-symmetric functions — it detects *angular* variation.

## Your Task

Implement \`vector_field(comps_fn)\` where \`comps_fn(point)\` returns the component list $[V^0(p), V^1(p)]$. The result should be a function $V$ such that $V(f)(*point)$ gives the directional derivative.
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

def vector_field(comps_fn):
    """Return V such that V(f)(*point) = sum_i comps_fn(point)[i] * ∂f/∂x_i."""
    pass

# Radial field: X = x ∂/∂x + y ∂/∂y
X = vector_field(lambda p: [p[0], p[1]])
# Rotation field: Y = -y ∂/∂x + x ∂/∂y
Y = vector_field(lambda p: [-p[1], p[0]])

f = lambda x, y: x**2 + y**2

print(f"{X(f)(1.0, 2.0):.4f}")  # 10.0000
print(f"{Y(f)(1.0, 2.0):.4f}")  # 0.0000

g = lambda x, y: x
print(f"{X(g)(1.0, 2.0):.4f}")  # 1.0000
print(f"{Y(g)(1.0, 2.0):.4f}")  # -2.0000
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

def vector_field(comps_fn):
    def V(f):
        def Vf(*point):
            comps = comps_fn(list(point))
            n = len(comps)
            return sum(comps[i] * partial(i, f)(*point) for i in range(n))
        return Vf
    return V

X = vector_field(lambda p: [p[0], p[1]])
Y = vector_field(lambda p: [-p[1], p[0]])

f = lambda x, y: x**2 + y**2

print(f"{X(f)(1.0, 2.0):.4f}")
print(f"{Y(f)(1.0, 2.0):.4f}")

g = lambda x, y: x
print(f"{X(g)(1.0, 2.0):.4f}")
print(f"{Y(g)(1.0, 2.0):.4f}")
`,
  tests: [
    {
      name: "radial field on x²+y² at (1,2) = 10",
      code: `{{FUNC}}
X = vector_field(lambda p: [p[0], p[1]])
f = lambda x, y: x**2 + y**2
print(f"{X(f)(1.0, 2.0):.4f}")`,
      expected: "10.0000\n",
    },
    {
      name: "rotation field on x²+y² at (1,2) = 0",
      code: `{{FUNC}}
Y = vector_field(lambda p: [-p[1], p[0]])
f = lambda x, y: x**2 + y**2
print(f"{abs(Y(f)(1.0, 2.0)):.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "radial field on x at (1,2) = 1",
      code: `{{FUNC}}
X = vector_field(lambda p: [p[0], p[1]])
g = lambda x, y: x
print(f"{X(g)(1.0, 2.0):.4f}")`,
      expected: "1.0000\n",
    },
    {
      name: "rotation field on x at (1,2) = -2",
      code: `{{FUNC}}
Y = vector_field(lambda p: [-p[1], p[0]])
g = lambda x, y: x
print(f"{Y(g)(1.0, 2.0):.4f}")`,
      expected: "-2.0000\n",
    },
  ],
};
