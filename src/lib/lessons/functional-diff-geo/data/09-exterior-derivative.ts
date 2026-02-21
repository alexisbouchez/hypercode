import type { Lesson } from "../../types";

export const exteriorDerivative: Lesson = {
  id: "exterior-derivative",
  title: "The Exterior Derivative",
  chapterId: "vector-fields",
  content: `# The Exterior Derivative

The **exterior derivative** $d$ generalizes the gradient, curl, and divergence into a single unified operator. It maps $p$-forms to $(p+1)$-forms.

For a **1-form** $\\omega = P\\,dx + Q\\,dy$ on $\\mathbb{R}^2$, the exterior derivative is a **2-form**:

$$d\\omega = \\left(\\frac{\\partial Q}{\\partial x} - \\frac{\\partial P}{\\partial y}\\right) dx \\wedge dy$$

The coefficient $\\frac{\\partial Q}{\\partial x} - \\frac{\\partial P}{\\partial y}$ is the **curl** of the vector field $(P, Q)$.

## Closed and Exact Forms

A form $\\omega$ is:
- **Closed** if $d\\omega = 0$ — the curl vanishes
- **Exact** if $\\omega = df$ for some function $f$ — it is the differential of something

Stokes's theorem says: $\\int_{\\partial M} \\omega = \\int_M d\\omega$.

## Examples

| One-form $\\omega$ | $d\\omega$ coefficient |
|---|---|
| $y\\,dx - x\\,dy$ | $-1 - 1 = -2$ |
| $x\\,dx + y\\,dy = d(\\frac{x^2+y^2}{2})$ | $0$ (exact) |
| $x^2\\,dx + xy\\,dy$ | $y - 0 = y$ |

## Your Task

Implement \`exterior_d(P, Q)\` that computes the exterior derivative of the 1-form $\\omega = P\\,dx + Q\\,dy$. Return a function of $(x, y)$ giving the coefficient of $dx \\wedge dy$.
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

def exterior_d(P, Q):
    """Exterior derivative of ω = P dx + Q dy.
    Returns a function (x, y) -> coefficient of dx∧dy = ∂Q/∂x - ∂P/∂y.
    """
    pass

# ω = y dx - x dy  (not closed)
print(f"{exterior_d(lambda x,y: y, lambda x,y: -x)(1.0, 2.0):.4f}")   # -2.0000

# ω = x dx + y dy  (exact: d(x²/2 + y²/2))
print(f"{exterior_d(lambda x,y: x, lambda x,y: y)(3.0, 4.0):.4f}")    # 0.0000

# ω = x² dx + xy dy
print(f"{exterior_d(lambda x,y: x**2, lambda x,y: x*y)(1.0, 3.0):.4f}")  # 3.0000
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

def exterior_d(P, Q):
    def curl(x, y):
        return partial(0, Q)(x, y) - partial(1, P)(x, y)
    return curl

print(f"{exterior_d(lambda x,y: y, lambda x,y: -x)(1.0, 2.0):.4f}")
print(f"{exterior_d(lambda x,y: x, lambda x,y: y)(3.0, 4.0):.4f}")
print(f"{exterior_d(lambda x,y: x**2, lambda x,y: x*y)(1.0, 3.0):.4f}")
`,
  tests: [
    {
      name: "d(y dx - x dy) = -2",
      code: `{{FUNC}}
print(f"{exterior_d(lambda x,y: y, lambda x,y: -x)(1.0, 2.0):.4f}")`,
      expected: "-2.0000\n",
    },
    {
      name: "d(x dx + y dy) = 0 (exact form)",
      code: `{{FUNC}}
print(f"{exterior_d(lambda x,y: x, lambda x,y: y)(3.0, 4.0):.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "d(x² dx + xy dy) = y at (1,3)",
      code: `{{FUNC}}
print(f"{exterior_d(lambda x,y: x**2, lambda x,y: x*y)(1.0, 3.0):.4f}")`,
      expected: "3.0000\n",
    },
    {
      name: "d(0 dx + x dy) = 1",
      code: `{{FUNC}}
print(f"{exterior_d(lambda x,y: 0, lambda x,y: x)(2.0, 2.0):.4f}")`,
      expected: "1.0000\n",
    },
  ],
};
