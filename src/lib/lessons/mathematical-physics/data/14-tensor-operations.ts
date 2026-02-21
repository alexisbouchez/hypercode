import type { Lesson } from "../../types";

export const tensorOperations: Lesson = {
  id: "tensor-operations",
  title: "Tensor Operations",
  chapterId: "geometry",
  content: `# Tensor Operations

**Tensors** are the natural language of modern physics — from the stress tensor in continuum mechanics to the Riemann curvature tensor in general relativity.

## Transformation Law

A rank-2 contravariant tensor transforms as:

$$T'^{ij} = \\Lambda^i_{\;k}\\, \\Lambda^j_{\;l}\\, T^{kl}$$

where $\\Lambda$ is the transformation matrix (Jacobian). This is just a double matrix multiplication.

## The Metric Tensor

The **metric tensor** $g_{ij}$ encodes the geometry of space. It raises and lowers indices:

$$T_i = g_{ij}\\, T^j, \\qquad T^i = g^{ij}\\, T_j$$

where $g^{ij}$ is the inverse metric. In flat 3D Cartesian space $g_{ij} = \\delta_{ij}$ (identity).

## Polar Coordinates (2D)

In polar coordinates $(r, \\theta)$, the metric is:

$$g = \\begin{pmatrix} 1 & 0 \\\\ 0 & r^2 \\end{pmatrix}$$

The inverse is $g^{rr} = 1$, $g^{\\theta\\theta} = 1/r^2$.

## Christoffel Symbols

The **Christoffel symbols** encode how basis vectors change across the manifold:

$$\\Gamma^k_{\;ij} = \\frac{1}{2}\\, g^{kl}\\bigl(\\partial_i g_{jl} + \\partial_j g_{il} - \\partial_l g_{ij}\\bigr)$$

For 2D polar coordinates, the non-zero Christoffel symbols are:

$$\\Gamma^r_{\;\\theta\\theta} = -r, \\qquad \\Gamma^\\theta_{\;r\\theta} = \\Gamma^\\theta_{\;\\theta r} = \\frac{1}{r}$$

All other components vanish. These give rise to the fictitious forces (centripetal, Coriolis) in rotating frames.

## Index Raising

Given a covariant tensor $T_{ij}$ and the inverse metric $g^{ij}$, we raise the first index:

$$T^i_{\;j} = g^{ik}\\, T_{kj}$$

## Implementation

- \`tensor_contract(A, B)\` — matrix multiply two 2D lists (works for any square size)
- \`metric_polar(r)\` — returns the 2x2 polar metric as a list of lists
- \`christoffel_polar_r_theta_theta(r)\` — returns $\\Gamma^r_{\;\\theta\\theta} = -r$
- \`christoffel_polar_theta_r_theta(r)\` — returns $\\Gamma^\\theta_{\;r\\theta} = 1/r$
- \`raise_index(T_lower, g_inv)\` — computes $T^i_{\;j} = g^{ik} T_{kj}$

\`\`\`python
def metric_polar(r):
    return [[1.0, 0.0], [0.0, r**2]]

def christoffel_polar_r_theta_theta(r):
    return -r
\`\`\`
`,
  starterCode: `import math

def tensor_contract(A, B):
    pass

def metric_polar(r):
    pass

def christoffel_polar_r_theta_theta(r):
    pass

def christoffel_polar_theta_r_theta(r):
    pass

def raise_index(T_lower, g_inv):
    pass
`,
  solution: `import math

def tensor_contract(A, B):
    n = len(A)
    m = len(B[0])
    k = len(B)
    C = [[0.0] * m for _ in range(n)]
    for i in range(n):
        for j in range(m):
            for l in range(k):
                C[i][j] += A[i][l] * B[l][j]
    return C

def metric_polar(r):
    return [[1.0, 0.0], [0.0, r**2]]

def christoffel_polar_r_theta_theta(r):
    return -r

def christoffel_polar_theta_r_theta(r):
    return 1.0 / r

def raise_index(T_lower, g_inv):
    n = len(T_lower)
    T_upper = [[0.0] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            for k in range(n):
                T_upper[i][j] += g_inv[i][k] * T_lower[k][j]
    return T_upper
`,
  tests: [
    {
      name: "christoffel_polar_r_theta_theta(2.0) = -r = -2.0",
      expected: "-2.0000\n",
      code: `{{FUNC}}\nprint(f"{christoffel_polar_r_theta_theta(2.0):.4f}")`,
    },
    {
      name: "christoffel_polar_theta_r_theta(2.0) = 1/r = 0.5",
      expected: "0.5000\n",
      code: `{{FUNC}}\nprint(f"{christoffel_polar_theta_r_theta(2.0):.4f}")`,
    },
    {
      name: "metric_polar(3.0)[1][1] = r^2 = 9.0",
      expected: "9.0000\n",
      code: `{{FUNC}}\nprint(f"{metric_polar(3.0)[1][1]:.4f}")`,
    },
    {
      name: "tensor_contract([[1,2],[3,4]], [[5,6],[7,8]])[0][0] = 19",
      expected: "19.0000\n",
      code: `{{FUNC}}\nprint(f"{tensor_contract([[1,2],[3,4]], [[5,6],[7,8]])[0][0]:.4f}")`,
    },
  ],
};
