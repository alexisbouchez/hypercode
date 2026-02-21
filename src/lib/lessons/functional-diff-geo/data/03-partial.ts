import type { Lesson } from "../../types";

export const partialDerivative: Lesson = {
  id: "partial-derivative",
  title: "Partial Derivatives",
  chapterId: "functional-foundations",
  content: `# Partial Derivatives

Functions in differential geometry often take multiple arguments — coordinates on a manifold. The **partial derivative** $\\partial_i f$ differentiates $f$ with respect to its $i$-th argument while holding all others fixed.

In functional notation, $\\partial_i$ is itself a higher-order function:

$$\\partial_i f(x_0, x_1, \\ldots) = \\lim_{h \\to 0} \\frac{f(\\ldots, x_i + h, \\ldots) - f(\\ldots, x_i - h, \\ldots)}{2h}$$

## The Lagrangian

The book opens with the Lagrange equations of motion. For a Lagrangian $L(t, q, \\dot{q})$:

$$\\frac{d}{dt}(\\partial_2 L) - \\partial_1 L = 0$$

Here $\\partial_1 L$ is the partial derivative of $L$ with respect to its second argument (position $q$), and $\\partial_2 L$ is the partial with respect to its third argument (velocity $\\dot{q}$). Index 0 is time.

For the harmonic oscillator $L = \\frac{1}{2}\\dot{q}^2 - \\frac{1}{2}kq^2$:
- $\\partial_1 L = -kq$
- $\\partial_2 L = \\dot{q}$

## Your Task

Implement \`partial(i, f)\` that returns the partial derivative of \`f\` with respect to argument \`i\` (0-indexed).
`,
  starterCode: `def partial(i, f, h=1e-7):
    pass

# f(x, y) = x^2 * y
f = lambda x, y: x**2 * y

df_dx = partial(0, f)   # 2xy
df_dy = partial(1, f)   # x^2

print(f"{df_dx(2.0, 3.0):.4f}")  # 12.0000
print(f"{df_dy(2.0, 3.0):.4f}")  # 4.0000

# Lagrangian: L(t, q, qdot) = 0.5*qdot^2 - 0.5*q^2
L = lambda t, q, qdot: 0.5 * qdot**2 - 0.5 * q**2
dL_dq    = partial(1, L)   # -q
dL_dqdot = partial(2, L)   # qdot

print(f"{dL_dq(0.0, 1.0, 2.0):.4f}")    # -1.0000
print(f"{dL_dqdot(0.0, 1.0, 2.0):.4f}") # 2.0000
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

f = lambda x, y: x**2 * y
df_dx = partial(0, f)
df_dy = partial(1, f)

print(f"{df_dx(2.0, 3.0):.4f}")
print(f"{df_dy(2.0, 3.0):.4f}")

L = lambda t, q, qdot: 0.5 * qdot**2 - 0.5 * q**2
dL_dq    = partial(1, L)
dL_dqdot = partial(2, L)

print(f"{dL_dq(0.0, 1.0, 2.0):.4f}")
print(f"{dL_dqdot(0.0, 1.0, 2.0):.4f}")
`,
  tests: [
    {
      name: "∂(x²y)/∂x at (2,3) = 12",
      code: `{{FUNC}}
print(f"{partial(0, lambda x, y: x**2 * y)(2.0, 3.0):.4f}")`,
      expected: "12.0000\n",
    },
    {
      name: "∂(x²y)/∂y at (2,3) = 4",
      code: `{{FUNC}}
print(f"{partial(1, lambda x, y: x**2 * y)(2.0, 3.0):.4f}")`,
      expected: "4.0000\n",
    },
    {
      name: "∂(xy)/∂x at (3,4) = 4",
      code: `{{FUNC}}
print(f"{partial(0, lambda x, y: x*y)(3.0, 4.0):.4f}")`,
      expected: "4.0000\n",
    },
    {
      name: "∂(x²+y²)/∂y at (1,2) = 4",
      code: `{{FUNC}}
print(f"{partial(1, lambda x, y: x**2 + y**2)(1.0, 2.0):.4f}")`,
      expected: "4.0000\n",
    },
  ],
};
