import type { Lesson } from "../../types";

export const jacobian: Lesson = {
  id: "jacobian",
  title: "The Jacobian Matrix",
  chapterId: "manifolds",
  content: `# The Jacobian Matrix

When we change coordinates on a manifold, vectors and tensors transform. The transformation is governed by the **Jacobian** — the matrix of all partial derivatives of the coordinate change.

For a map $F: \\mathbb{R}^n \\to \\mathbb{R}^m$ with $F = (F_0, F_1, \\ldots, F_{m-1})$, the Jacobian at a point $p$ is:

$$J_{ij} = \\frac{\\partial F_i}{\\partial x_j}(p)$$

## Example: Polar-to-Rectangular

The map $F(r, \\theta) = (r\\cos\\theta,\\; r\\sin\\theta)$ has Jacobian:

$$J = \\begin{pmatrix} \\cos\\theta & -r\\sin\\theta \\\\ \\sin\\theta & r\\cos\\theta \\end{pmatrix}$$

At $(r=1, \\theta=0)$: $J = \\begin{pmatrix} 1 & 0 \\\\ 0 & 1 \\end{pmatrix}$ — the identity.

At $(r=2, \\theta=0)$: $J = \\begin{pmatrix} 1 & 0 \\\\ 0 & 2 \\end{pmatrix}$ — $\\partial y / \\partial \\theta = r = 2$.

## Determinant

The determinant $\\det(J)$ is the **local volume scale factor**: how much the map stretches areas. For polar coordinates, $\\det(J) = r$, so the area element in polar coords is $r\\,dr\\,d\\theta$.

## Your Task

Implement \`jacobian(F, n)\` that takes a vector-valued function $F: \\mathbb{R}^n \\to \\mathbb{R}^m$ and returns a function \`J(point)\` that computes the Jacobian matrix $J[i][j] = \\partial F_i / \\partial x_j$ at that point.
`,
  starterCode: `import math

def jacobian(F, n, h=1e-7):
    """Return J(point) where J[i][j] = ∂F_i/∂x_j."""
    pass

# Polar -> rectangular: (r, theta) -> (r*cos(theta), r*sin(theta))
F = lambda c: [c[0]*math.cos(c[1]), c[0]*math.sin(c[1])]
J = jacobian(F, 2)

j0 = J([1.0, 0.0])   # At (r=1, theta=0)
print(f"{j0[0][0]:.4f}")  # ∂x/∂r = cos(0) = 1.0
print(f"{j0[0][1]:.4f}")  # ∂x/∂θ = -r*sin(0) = 0.0
print(f"{j0[1][0]:.4f}")  # ∂y/∂r = sin(0) = 0.0
print(f"{j0[1][1]:.4f}")  # ∂y/∂θ = r*cos(0) = 1.0
`,
  solution: `import math

def jacobian(F, n, h=1e-7):
    def J(point):
        m = len(F(point))
        result = [[0.0]*n for _ in range(m)]
        for j in range(n):
            p_plus  = list(point)
            p_minus = list(point)
            p_plus[j]  += h
            p_minus[j] -= h
            Fp = F(p_plus)
            Fm = F(p_minus)
            for i in range(m):
                result[i][j] = (Fp[i] - Fm[i]) / (2 * h)
        return result
    return J

F = lambda c: [c[0]*math.cos(c[1]), c[0]*math.sin(c[1])]
J = jacobian(F, 2)

j0 = J([1.0, 0.0])
print(f"{j0[0][0]:.4f}")
print(f"{j0[0][1]:.4f}")
print(f"{j0[1][0]:.4f}")
print(f"{j0[1][1]:.4f}")
`,
  tests: [
    {
      name: "∂x/∂r at (r=1, θ=0) = 1",
      code: `{{FUNC}}
F = lambda c: [c[0]*math.cos(c[1]), c[0]*math.sin(c[1])]
J = jacobian(F, 2)
print(f"{J([1.0, 0.0])[0][0]:.4f}")`,
      expected: "1.0000\n",
    },
    {
      name: "∂x/∂θ at (r=1, θ=0) = 0",
      code: `{{FUNC}}
F = lambda c: [c[0]*math.cos(c[1]), c[0]*math.sin(c[1])]
J = jacobian(F, 2)
print(f"{J([1.0, 0.0])[0][1]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "∂x/∂r at (r=1, θ=π/2) = 0",
      code: `{{FUNC}}
import math
F = lambda c: [c[0]*math.cos(c[1]), c[0]*math.sin(c[1])]
J = jacobian(F, 2)
print(f"{J([1.0, math.pi/2])[0][0]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "∂y/∂θ at (r=2, θ=0) = 2",
      code: `{{FUNC}}
F = lambda c: [c[0]*math.cos(c[1]), c[0]*math.sin(c[1])]
J = jacobian(F, 2)
print(f"{J([2.0, 0.0])[1][1]:.4f}")`,
      expected: "2.0000\n",
    },
  ],
};
