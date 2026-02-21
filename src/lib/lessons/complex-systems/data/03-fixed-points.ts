import type { Lesson } from "../../types";

export const fixedPoints: Lesson = {
  id: "fixed-points",
  title: "Fixed Points and Stability",
  chapterId: "dynamical-systems",
  content: `# Fixed Points and Stability

A **fixed point** (or equilibrium) of a dynamical system is a state that does not change over time. Understanding their stability tells us whether the system returns to equilibrium after a small perturbation.

## 1D Systems

For $\\dot{x} = f(x)$, a fixed point $x^*$ satisfies $f(x^*) = 0$.

**Stability criterion:**
- $f'(x^*) < 0$ → **stable** (perturbations decay)
- $f'(x^*) > 0$ → **unstable** (perturbations grow)

We estimate the derivative numerically using the central difference:

$$f'(x) \\approx \\frac{f(x+h) - f(x-h)}{2h}$$

## 2D Systems

For $\\dot{x} = f(x, y)$ and $\\dot{y} = g(x, y)$, stability is determined by the **Jacobian matrix** at the fixed point:

$$J = \\begin{pmatrix} \\partial f/\\partial x & \\partial f/\\partial y \\\\ \\partial g/\\partial x & \\partial g/\\partial y \\end{pmatrix}$$

The eigenvalues of $J$ determine the behaviour. From the **characteristic polynomial** $\\lambda^2 - \\tau\\lambda + \\Delta = 0$ (where $\\tau = J_{11}+J_{22}$ is the trace and $\\Delta = J_{11}J_{22} - J_{12}J_{21}$ is the determinant):

$$\\lambda_{1,2} = \\frac{\\tau \\pm \\sqrt{\\tau^2 - 4\\Delta}}{2}$$

| Condition | Classification |
|---|---|
| $\\Delta < 0$ | **Saddle point** (always unstable) |
| $\\tau < 0,\\ \\Delta > 0,\\ \\tau^2 > 4\\Delta$ | **Stable node** |
| $\\tau < 0,\\ \\Delta > 0,\\ \\tau^2 < 4\\Delta$ | **Stable spiral** |
| $\\tau > 0$ | **Unstable** |
| $\\tau = 0,\\ \\Delta > 0$ | **Centre** (neutrally stable) |

## Your Task

Implement:
- \`numerical_derivative(f_func, x, h=1e-6)\` — central difference approximation of $f'(x)$
- \`is_stable_1d(f_func, x_star)\` — returns \`True\` if the fixed point is stable
- \`jacobian_eigenvalues(J11, J12, J21, J22)\` — returns \`(lambda1, lambda2)\` as real floats (the real parts, sorted descending); for complex eigenvalues return the real part $\\tau/2$
`,
  starterCode: `import math

def numerical_derivative(f_func, x, h=1e-6):
    pass

def is_stable_1d(f_func, x_star):
    pass

def jacobian_eigenvalues(J11, J12, J21, J22):
    pass
`,
  solution: `import math

def numerical_derivative(f_func, x, h=1e-6):
    return (f_func(x + h) - f_func(x - h)) / (2 * h)

def is_stable_1d(f_func, x_star):
    return numerical_derivative(f_func, x_star) < 0

def jacobian_eigenvalues(J11, J12, J21, J22):
    tau = J11 + J22
    delta = J11 * J22 - J12 * J21
    discriminant = tau**2 - 4 * delta
    if discriminant >= 0:
        l1 = (tau + math.sqrt(discriminant)) / 2
        l2 = (tau - math.sqrt(discriminant)) / 2
    else:
        l1 = tau / 2
        l2 = tau / 2
    if l1 >= l2:
        return (l1, l2)
    else:
        return (l2, l1)
`,
  tests: [
    {
      name: "numerical_derivative of x^2-1 at x=1 is 2",
      expected: "2.0000\n",
      code: `{{FUNC}}
print(f"{numerical_derivative(lambda x: x**2 - 1, 1.0):.4f}")`,
    },
    {
      name: "is_stable_1d of x*(1-x) at x=1 is True",
      expected: "True\n",
      code: `{{FUNC}}
print(is_stable_1d(lambda x: x*(1-x), 1.0))`,
    },
    {
      name: "jacobian_eigenvalues(-1,0,0,-2)[0] is -1 (diagonal, larger eigenvalue)",
      expected: "-1.0000\n",
      code: `{{FUNC}}
print(f"{jacobian_eigenvalues(-1, 0, 0, -2)[0]:.4f}")`,
    },
    {
      name: "jacobian_eigenvalues(0,-1,1,0)[0] real part is 0 (centre)",
      expected: "0.0000\n",
      code: `{{FUNC}}
print(f"{jacobian_eigenvalues(0, -1, 1, 0)[0]:.4f}")`,
    },
  ],
};
