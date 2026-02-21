import type { Lesson } from "../../types";

export const christoffel: Lesson = {
  id: "christoffel",
  title: "Christoffel Symbols",
  chapterId: "connections-curvature",
  content: `# Christoffel Symbols

To differentiate a vector field on a manifold, we need a **connection** — a rule for comparing vectors at different points. The most natural choice for a Riemannian manifold is the **Levi-Civita connection**, encoded by the **Christoffel symbols** $\\Gamma^k_{ij}$.

From the metric $g_{ij}$, the Christoffel symbols are:

$$\\Gamma^k_{ij} = \\frac{1}{2} g^{kl}\\left(\\frac{\\partial g_{lj}}{\\partial x^i} + \\frac{\\partial g_{li}}{\\partial x^j} - \\frac{\\partial g_{ij}}{\\partial x^l}\\right)$$

where $g^{kl}$ is the **inverse metric**.

## Sphere Example

For the unit sphere with metric $g = \\text{diag}(1, \\sin^2\\theta)$:

$$\\Gamma^\\theta_{\\phi\\phi} = -\\sin\\theta\\cos\\theta$$
$$\\Gamma^\\phi_{\\theta\\phi} = \\Gamma^\\phi_{\\phi\\theta} = \\cot\\theta$$

All others vanish. At $\\theta = \\pi/4$:
- $\\Gamma^\\theta_{\\phi\\phi} = -\\sin(\\pi/4)\\cos(\\pi/4) = -0.5$
- $\\Gamma^\\phi_{\\theta\\phi} = \\cot(\\pi/4) = 1.0$

## Flat Space

On flat $\\mathbb{R}^n$ with $g_{ij} = \\delta_{ij}$, all Christoffel symbols vanish: $\\Gamma^k_{ij} = 0$.

## Your Task

Implement \`christoffel(g_fn)\` that takes a metric function and returns a function \`at(point)\` that computes the $2 \\times 2 \\times 2$ array \`Gamma[k][i][j]\` = $\\Gamma^k_{ij}$ at that point.
`,
  starterCode: `import math

def partial(i, f, h=1e-5):
    def df(*args):
        args = list(args)
        p = args[:]
        m = args[:]
        p[i] += h
        m[i] -= h
        return (f(*p) - f(*m)) / (2 * h)
    return df

def christoffel(g_fn, n=2):
    """Return at(point) -> Gamma[k][i][j] = Γ^k_ij."""
    def at(point):
        G = g_fn(point)
        # 2x2 inverse
        det = G[0][0]*G[1][1] - G[0][1]*G[1][0]
        Ginv = [[G[1][1]/det, -G[0][1]/det],
                [-G[1][0]/det, G[0][0]/det]]

        # Metric derivatives: dG[k][a][b] = ∂g_ab/∂x_k
        dG = [[[0.0]*n for _ in range(n)] for _ in range(n)]
        for k in range(n):
            for a in range(n):
                for b in range(n):
                    pass  # fill in

        # Gamma[k][i][j] = 0.5 * sum_l Ginv[k][l] * (dG[i][l][j] + dG[j][l][i] - dG[l][i][j])
        Gamma = [[[0.0]*n for _ in range(n)] for _ in range(n)]
        # fill in
        return Gamma
    return at

# Test: flat metric -> all zeros
at_flat = christoffel(lambda p: [[1, 0], [0, 1]])
G_flat = at_flat([0.0, 0.0])
print(f"{G_flat[0][0][0]:.4f}")  # 0.0000

# Test: sphere metric at θ=π/4
at_sphere = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
G_sp = at_sphere([math.pi/4, 0.0])
print(f"{G_sp[0][1][1]:.4f}")   # Γ^θ_φφ = -sin(π/4)cos(π/4) = -0.5000
print(f"{G_sp[1][0][1]:.4f}")   # Γ^φ_θφ = cot(π/4) = 1.0000
`,
  solution: `import math

def partial(i, f, h=1e-5):
    def df(*args):
        args = list(args)
        p = args[:]
        m = args[:]
        p[i] += h
        m[i] -= h
        return (f(*p) - f(*m)) / (2 * h)
    return df

def christoffel(g_fn, n=2):
    def at(point):
        G = g_fn(point)
        det = G[0][0]*G[1][1] - G[0][1]*G[1][0]
        Ginv = [[G[1][1]/det, -G[0][1]/det],
                [-G[1][0]/det, G[0][0]/det]]

        dG = [[[0.0]*n for _ in range(n)] for _ in range(n)]
        for k in range(n):
            for a in range(n):
                for b in range(n):
                    def g_ab_fn(a=a, b=b):
                        def f(*args):
                            return g_fn(list(args))[a][b]
                        return f
                    dG[k][a][b] = partial(k, g_ab_fn())(*point)

        Gamma = [[[0.0]*n for _ in range(n)] for _ in range(n)]
        for k in range(n):
            for i in range(n):
                for j in range(n):
                    Gamma[k][i][j] = 0.5 * sum(
                        Ginv[k][l] * (dG[i][l][j] + dG[j][l][i] - dG[l][i][j])
                        for l in range(n)
                    )
        return Gamma
    return at

at_flat = christoffel(lambda p: [[1, 0], [0, 1]])
G_flat = at_flat([0.0, 0.0])
print(f"{G_flat[0][0][0]:.4f}")

at_sphere = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
G_sp = at_sphere([math.pi/4, 0.0])
print(f"{G_sp[0][1][1]:.4f}")
print(f"{G_sp[1][0][1]:.4f}")
`,
  tests: [
    {
      name: "flat metric: all Christoffel = 0",
      code: `{{FUNC}}
at = christoffel(lambda p: [[1, 0], [0, 1]])
G = at([0.0, 0.0])
print(f"{G[0][0][0]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "sphere: Γ^θ_φφ = -0.5 at θ=π/4",
      code: `{{FUNC}}
import math
at = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
G = at([math.pi/4, 0.0])
print(f"{G[0][1][1]:.4f}")`,
      expected: "-0.5000\n",
    },
    {
      name: "sphere: Γ^φ_θφ = 1.0 at θ=π/4",
      code: `{{FUNC}}
import math
at = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
G = at([math.pi/4, 0.0])
print(f"{G[1][0][1]:.4f}")`,
      expected: "1.0000\n",
    },
    {
      name: "sphere: Γ^φ_φθ = 1.0 at θ=π/4 (symmetric)",
      code: `{{FUNC}}
import math
at = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
G = at([math.pi/4, 0.0])
print(f"{G[1][1][0]:.4f}")`,
      expected: "1.0000\n",
    },
  ],
};
