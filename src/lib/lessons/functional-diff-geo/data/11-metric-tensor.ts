import type { Lesson } from "../../types";

export const metricTensor: Lesson = {
  id: "metric-tensor",
  title: "The Metric Tensor",
  chapterId: "connections-curvature",
  content: `# The Metric Tensor

A **metric tensor** $g$ gives the manifold its geometry — it defines lengths, angles, and volumes. At each point $p$, $g_p$ is a symmetric bilinear form on tangent vectors:

$$g(u, v) = \\sum_{i,j} g_{ij}(p)\\, u^i v^j$$

where $g_{ij}$ is the metric **component matrix**.

## Examples

**Euclidean metric** on $\\mathbb{R}^2$: $g_{ij} = \\delta_{ij}$ (identity matrix)
$$g(u, v) = u_x v_x + u_y v_y$$

**Sphere of radius $R$** in colatitude $\\theta$, longitude $\\phi$:
$$g = \\begin{pmatrix} R^2 & 0 \\\\ 0 & R^2\\sin^2\\theta \\end{pmatrix}$$

The $\\phi$-direction shrinks near the poles ($\\theta \\to 0$ or $\\pi$) because circles of latitude get smaller.

## Length of a Vector

$$|v|^2 = g(v, v) = \\sum_{i,j} g_{ij} v^i v^j$$

For $v = [3, 4]$ on flat $\\mathbb{R}^2$: $|v|^2 = 9 + 16 = 25$, so $|v| = 5$.

## Lagrangian Connection

The kinetic energy Lagrangian for free motion on a manifold with metric $g$ is:

$$L = \\frac{1}{2} m\\, g(\\dot{q}, \\dot{q}) = \\frac{m}{2}\\sum_{ij} g_{ij}(q)\\dot{q}^i \\dot{q}^j$$

This connects geometry to dynamics.

## Your Task

Implement \`metric(G_fn)\` where \`G_fn(point)\` returns the $n \\times n$ matrix $[g_{ij}]$. Return a function \`g\` such that \`g(u, v)(*point)\` computes $g_p(u, v)$.
`,
  starterCode: `import math

def metric(G_fn):
    """Return g such that g(u, v)(*point) = sum_ij G_fn(point)[i][j] * u[i] * v[j]."""
    pass

# Euclidean metric: g = identity
g_flat = metric(lambda p: [[1, 0], [0, 1]])

print(f"{g_flat([1, 0], [0, 1])(3.0, 4.0):.4f}")   # 0.0000  (orthogonal)
print(f"{g_flat([3, 4], [3, 4])(0.0, 0.0):.4f}")   # 25.0000 (length squared)

# Sphere metric (R=1): g = diag(1, sin²θ)
g_sphere = metric(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
theta = math.pi / 2   # equator

print(f"{g_sphere([1, 0], [1, 0])(theta, 0.0):.4f}")  # 1.0000
print(f"{g_sphere([0, 1], [0, 1])(theta, 0.0):.4f}")  # sin²(π/2) = 1.0000
`,
  solution: `import math

def metric(G_fn):
    def g(u, v):
        def inner(*point):
            G = G_fn(list(point))
            n = len(u)
            return sum(G[i][j] * u[i] * v[j] for i in range(n) for j in range(n))
        return inner
    return g

g_flat = metric(lambda p: [[1, 0], [0, 1]])

print(f"{g_flat([1, 0], [0, 1])(3.0, 4.0):.4f}")
print(f"{g_flat([3, 4], [3, 4])(0.0, 0.0):.4f}")

g_sphere = metric(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
theta = math.pi / 2

print(f"{g_sphere([1, 0], [1, 0])(theta, 0.0):.4f}")
print(f"{g_sphere([0, 1], [0, 1])(theta, 0.0):.4f}")
`,
  tests: [
    {
      name: "flat metric: [1,0]·[0,1] = 0",
      code: `{{FUNC}}
g = metric(lambda p: [[1, 0], [0, 1]])
print(f"{g([1, 0], [0, 1])(3.0, 4.0):.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "flat metric: |[3,4]|² = 25",
      code: `{{FUNC}}
g = metric(lambda p: [[1, 0], [0, 1]])
print(f"{g([3, 4], [3, 4])(0.0, 0.0):.4f}")`,
      expected: "25.0000\n",
    },
    {
      name: "sphere metric: θ-vector at equator has length 1",
      code: `{{FUNC}}
import math
g = metric(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
print(f"{g([1, 0], [1, 0])(math.pi/2, 0.0):.4f}")`,
      expected: "1.0000\n",
    },
    {
      name: "sphere metric: φ-vector at equator has length 1",
      code: `{{FUNC}}
import math
g = metric(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
print(f"{g([0, 1], [0, 1])(math.pi/2, 0.0):.4f}")`,
      expected: "1.0000\n",
    },
  ],
};
