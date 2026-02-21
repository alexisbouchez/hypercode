import type { Lesson } from "../../types";

export const lieBracket: Lesson = {
  id: "lie-bracket",
  title: "The Lie Bracket",
  chapterId: "vector-fields",
  content: `# The Lie Bracket

Two vector fields $V$ and $W$ can be applied in sequence to a function $f$: first apply $W$ to get $W[f]$, then apply $V$ to get $V[W[f]]$. In general, the result depends on the order:

$$V[W[f]] \\neq W[V[f]]$$

The **Lie bracket** $[V, W]$ measures this failure to commute:

$$[V, W][f] = V[W[f]] - W[V[f]]$$

The Lie bracket is itself a vector field! This is a fundamental result: the space of vector fields is closed under the Lie bracket.

## In Coordinates

For $V = V^i \\partial_i$ and $W = W^j \\partial_j$:

$$[V, W]^k = V^i \\partial_i W^k - W^i \\partial_i V^k$$

## Geometric Meaning

The Lie bracket measures the *obstruction to commutativity* of flows. If you flow along $V$ for time $\\epsilon$, then along $W$ for $\\epsilon$, then back along $V$ for $\\epsilon$, then back along $W$ for $\\epsilon$, you end up displaced by $\\epsilon^2 [V, W]$ from where you started.

## Examples

| $V$ | $W$ | $[V, W]$ |
|-----|-----|----------|
| $\\partial/\\partial x$ | $\\partial/\\partial y$ | $0$ (commute) |
| $y\\partial/\\partial x$ | $x\\partial/\\partial y$ | $y\\partial/\\partial y - x\\partial/\\partial x$ |

## Your Task

Implement \`lie_bracket(V, W)\` where $V$ and $W$ are vector fields (as built by \`vector_field\`). Return the Lie bracket $[V, W]$ as a vector field.
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
    def V(f):
        def Vf(*point):
            comps = comps_fn(list(point))
            n = len(comps)
            return sum(comps[i] * partial(i, f)(*point) for i in range(n))
        return Vf
    return V

def lie_bracket(V, W):
    """Return [V, W] = VW - WV as a vector field operator."""
    pass

# [∂/∂x, ∂/∂y] = 0 (partial derivatives commute)
d_dx = vector_field(lambda p: [1, 0])
d_dy = vector_field(lambda p: [0, 1])
bracket1 = lie_bracket(d_dx, d_dy)
f = lambda x, y: x**2 * y
print(f"{bracket1(f)(1.0, 2.0):.4f}")   # 0.0000

# [y∂/∂x, x∂/∂y](x²) at (1,2)
V = vector_field(lambda p: [p[1], 0])
W = vector_field(lambda p: [0, p[0]])
bracket2 = lie_bracket(V, W)
g = lambda x, y: x**2
print(f"{bracket2(g)(1.0, 2.0):.4f}")   # -2.0000
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

def lie_bracket(V, W):
    def VW(f):
        def VWf(*point):
            return V(W(f))(*point) - W(V(f))(*point)
        return VWf
    return VW

d_dx = vector_field(lambda p: [1, 0])
d_dy = vector_field(lambda p: [0, 1])
bracket1 = lie_bracket(d_dx, d_dy)
f = lambda x, y: x**2 * y
print(f"{bracket1(f)(1.0, 2.0):.4f}")

V = vector_field(lambda p: [p[1], 0])
W = vector_field(lambda p: [0, p[0]])
bracket2 = lie_bracket(V, W)
g = lambda x, y: x**2
print(f"{bracket2(g)(1.0, 2.0):.4f}")
`,
  tests: [
    {
      name: "[∂/∂x, ∂/∂y](x²y) at (1,2) = 0",
      code: `{{FUNC}}
d_dx = vector_field(lambda p: [1, 0])
d_dy = vector_field(lambda p: [0, 1])
b = lie_bracket(d_dx, d_dy)
print(f"{b(lambda x, y: x**2 * y)(1.0, 2.0):.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "[y∂/∂x, x∂/∂y](x²) at (1,2) = -2",
      code: `{{FUNC}}
V = vector_field(lambda p: [p[1], 0])
W = vector_field(lambda p: [0, p[0]])
b = lie_bracket(V, W)
print(f"{b(lambda x, y: x**2)(1.0, 2.0):.4f}")`,
      expected: "-2.0000\n",
    },
    {
      name: "[y∂/∂x, x∂/∂y](x²+y²) at (1,2) = 6",
      code: `{{FUNC}}
V = vector_field(lambda p: [p[1], 0])
W = vector_field(lambda p: [0, p[0]])
b = lie_bracket(V, W)
print(f"{b(lambda x, y: x**2 + y**2)(1.0, 2.0):.4f}")`,
      expected: "6.0000\n",
    },
    {
      name: "[y∂/∂x, x∂/∂y](xy) at (1,1) = 0",
      code: `{{FUNC}}
V = vector_field(lambda p: [p[1], 0])
W = vector_field(lambda p: [0, p[0]])
b = lie_bracket(V, W)
print(f"{b(lambda x, y: x * y)(1.0, 1.0):.4f}")`,
      expected: "0.0000\n",
    },
  ],
};
