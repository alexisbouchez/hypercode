import type { Lesson } from "../../types";

export const riemannCurvature: Lesson = {
  id: "riemann-curvature",
  title: "Riemann Curvature Tensor",
  chapterId: "connections-curvature",
  content: `# Riemann Curvature Tensor

The **Riemann curvature tensor** $R$ measures how much parallel transport around a small loop rotates vectors. This is the quantitative measure of the curvature of a manifold.

The Riemann tensor is defined in terms of the Lie bracket and covariant derivative:

$$R(u, v)w = \\nabla_u \\nabla_v w - \\nabla_v \\nabla_u w - \\nabla_{[u,v]} w$$

In components, using the Christoffel symbols:

$$R^k{}_{l ij} = \\partial_i \\Gamma^k_{jl} - \\partial_j \\Gamma^k_{il} + \\sum_m\\left(\\Gamma^k_{im}\\Gamma^m_{jl} - \\Gamma^k_{jm}\\Gamma^m_{il}\\right)$$

## Sphere Example

For the unit sphere with $g = \\text{diag}(1, \\sin^2\\theta)$:

$$R^\\theta{}_{\\phi\\theta\\phi} = \\sin^2\\theta$$

At $\\theta = \\pi/2$: $R^\\theta{}_{\\phi\\theta\\phi} = 1$.

At $\\theta = \\pi/4$: $R^\\theta{}_{\\phi\\theta\\phi} = \\sin^2(\\pi/4) = 0.5$.

## Gaussian Curvature

For a 2D surface, the Riemann tensor has one independent component. The **Gaussian curvature** is:

$$K = \\frac{R^\\theta{}_{\\phi\\theta\\phi}}{g_{\\phi\\phi}} = \\frac{\\sin^2\\theta}{\\sin^2\\theta} = 1$$

For the unit sphere, $K = 1$ everywhere — it is a surface of constant positive curvature.

Flat space has $K = 0$: all $\\Gamma = 0$, so all $R = 0$.

## Your Task

Implement \`riemann(g_fn)\` that returns a function \`at(point)\` computing $R[k][l][i][j] = R^k{}_{lij}$.
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

def riemann(g_fn, n=2):
    """Return at(point) -> R[k][l][i][j] = R^k_lij."""
    chris = christoffel(g_fn, n)
    def at(point):
        Gamma = chris(point)

        # ∂_i Gamma^k_jl at point
        dGamma = [[[[0.0]*n for _ in range(n)] for _ in range(n)] for _ in range(n)]
        for i in range(n):
            for k in range(n):
                for j in range(n):
                    for l in range(n):
                        pass  # fill in: dGamma[i][k][j][l] = ∂_i Gamma^k_jl

        # R^k_lij = ∂_i Γ^k_jl - ∂_j Γ^k_il + Γ^k_im Γ^m_jl - Γ^k_jm Γ^m_il
        R = [[[[0.0]*n for _ in range(n)] for _ in range(n)] for _ in range(n)]
        # fill in
        return R
    return at

# Flat space: all R = 0
at_flat = riemann(lambda p: [[1, 0], [0, 1]])
R_flat = at_flat([1.0, 1.0])
print(f"{R_flat[0][1][0][1]:.4f}")   # 0.0000

# Unit sphere at θ=π/2: R^θ_φθφ = sin²(π/2) = 1
at_sphere = riemann(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
R_sp = at_sphere([math.pi/2, 0.0])
print(f"{R_sp[0][1][0][1]:.4f}")     # 1.0000
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

def riemann(g_fn, n=2):
    chris = christoffel(g_fn, n)
    def at(point):
        Gamma = chris(point)
        dGamma = [[[[0.0]*n for _ in range(n)] for _ in range(n)] for _ in range(n)]
        for i in range(n):
            for k in range(n):
                for j2 in range(n):
                    for l in range(n):
                        def G_kjl(k=k, j2=j2, l=l):
                            def f(*args):
                                return chris(list(args))[k][j2][l]
                            return f
                        dGamma[i][k][j2][l] = partial(i, G_kjl())(*point)
        R = [[[[0.0]*n for _ in range(n)] for _ in range(n)] for _ in range(n)]
        for k in range(n):
            for l in range(n):
                for i in range(n):
                    for j in range(n):
                        R[k][l][i][j] = (
                            dGamma[i][k][j][l] - dGamma[j][k][i][l] +
                            sum(Gamma[k][i][m]*Gamma[m][j][l] - Gamma[k][j][m]*Gamma[m][i][l]
                                for m in range(n))
                        )
        return R
    return at

at_flat = riemann(lambda p: [[1, 0], [0, 1]])
R_flat = at_flat([1.0, 1.0])
print(f"{R_flat[0][1][0][1]:.4f}")

at_sphere = riemann(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
R_sp = at_sphere([math.pi/2, 0.0])
print(f"{R_sp[0][1][0][1]:.4f}")
`,
  tests: [
    {
      name: "flat space: R = 0",
      code: `{{FUNC}}
at = riemann(lambda p: [[1, 0], [0, 1]])
R = at([1.0, 1.0])
print(f"{R[0][1][0][1]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "unit sphere at θ=π/2: R^θ_φθφ = 1",
      code: `{{FUNC}}
import math
at = riemann(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
R = at([math.pi/2, 0.0])
print(f"{R[0][1][0][1]:.4f}")`,
      expected: "1.0000\n",
    },
    {
      name: "unit sphere at θ=π/4: R^θ_φθφ = 0.5",
      code: `{{FUNC}}
import math
at = riemann(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
R = at([math.pi/4, 0.0])
print(f"{R[0][1][0][1]:.4f}")`,
      expected: "0.5000\n",
    },
    {
      name: "antisymmetry: R^θ_φφθ = -1 at θ=π/2",
      code: `{{FUNC}}
import math
at = riemann(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
R = at([math.pi/2, 0.0])
print(f"{R[0][1][1][0]:.4f}")`,
      expected: "-1.0000\n",
    },
  ],
};
