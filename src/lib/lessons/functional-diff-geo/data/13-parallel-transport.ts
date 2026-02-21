import type { Lesson } from "../../types";

export const parallelTransport: Lesson = {
  id: "parallel-transport",
  title: "Parallel Transport",
  chapterId: "connections-curvature",
  content: `# Parallel Transport

When you move a vector along a curve on a manifold while keeping it "as parallel as possible", you perform **parallel transport**. This is the covariant version of "keeping a vector constant".

Given a curve $\\gamma(t)$ with velocity $\\dot{\\gamma}^i$, the parallel transport equation for a vector $V^k$ is:

$$\\frac{dV^k}{dt} + \\Gamma^k_{ij}\\, \\dot{\\gamma}^i\\, V^j = 0$$

Flat space: all $\\Gamma = 0$, so $dV/dt = 0$ — vectors don't change.

## Sphere Example: Transport Along a Meridian

Consider the unit sphere with $\\theta$ as colatitude and $\\phi$ as longitude. Transport the vector $V = [0, 1]$ (pointing east) along the meridian $\\phi = 0$, decreasing $\\theta$ from $\\pi/2$ toward $0$.

The curve: $\\gamma(t) = (\\pi/2 - t,\\; 0)$, so $\\dot{\\theta} = -1$, $\\dot{\\phi} = 0$.

The equations become:
$$\\frac{dV^\\theta}{dt} = 0$$
$$\\frac{dV^\\phi}{dt} = \\Gamma^\\phi_{\\theta\\phi}\\, V^\\phi = \\cot(\\theta)\\, V^\\phi$$

Starting from $V = [0, 1]$ at $\\theta = \\pi/2$, after time $t$:

$$V^\\phi(t) = \\frac{1}{\\cos t}$$

At $t = \\pi/3$: $V^\\phi = 1/\\cos(\\pi/3) = 2.0$. The coordinate component grows, but the **physical length** stays 1: $|V|^2 = \\sin^2(\\pi/6) \\cdot 4 = 1$.

## Your Task

Implement \`parallel_transport(gamma_dot, V0, christoffel_fn, t_end, steps)\` using RK4 integration. It should return the final transported vector.
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

def parallel_transport(p0, V0, gamma_dot_fn, christoffel_fn, t_end, steps=2000):
    """Transport vector V0 from p0 along curve gamma(t) for t in [0, t_end].
    gamma_dot_fn(t, point) -> [dγ^0/dt, dγ^1/dt]
    Returns final vector [V^0, V^1]. Use RK4 integration.
    """
    pass

# Flat R2: transport [1, 0] along x-axis — should stay [1, 0]
flat_chris = christoffel(lambda p: [[1, 0], [0, 1]])
V_flat = parallel_transport(
    [0.0, 0.0], [1.0, 0.0],
    lambda t, p: [1.0, 0.0],
    flat_chris, t_end=1.0
)
print(f"{V_flat[0]:.4f}")  # 1.0000
print(f"{V_flat[1]:.4f}")  # 0.0000
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

def parallel_transport(p0, V0, gamma_dot_fn, christoffel_fn, t_end, steps=2000):
    n = len(V0)
    p = list(p0)
    V = list(V0)
    dt = t_end / steps

    def deriv(t, p, V):
        gd = gamma_dot_fn(t, p)
        Gamma = christoffel_fn(p)
        dV = [-sum(Gamma[k][i][j] * gd[i] * V[j]
                   for i in range(n) for j in range(n))
              for k in range(n)]
        return list(gd), dV

    for step in range(steps):
        t = step * dt
        dp1, dV1 = deriv(t, p, V)
        p2 = [p[i] + 0.5*dt*dp1[i] for i in range(n)]
        V2 = [V[k] + 0.5*dt*dV1[k] for k in range(n)]
        dp2, dV2 = deriv(t + 0.5*dt, p2, V2)
        p3 = [p[i] + 0.5*dt*dp2[i] for i in range(n)]
        V3 = [V[k] + 0.5*dt*dV2[k] for k in range(n)]
        dp3, dV3 = deriv(t + 0.5*dt, p3, V3)
        p4 = [p[i] + dt*dp3[i] for i in range(n)]
        V4 = [V[k] + dt*dV3[k] for k in range(n)]
        dp4, dV4 = deriv(t + dt, p4, V4)
        p = [p[i] + dt*(dp1[i] + 2*dp2[i] + 2*dp3[i] + dp4[i])/6 for i in range(n)]
        V = [V[k] + dt*(dV1[k] + 2*dV2[k] + 2*dV3[k] + dV4[k])/6 for k in range(n)]
    return V

flat_chris = christoffel(lambda p: [[1, 0], [0, 1]])
V_flat = parallel_transport(
    [0.0, 0.0], [1.0, 0.0],
    lambda t, p: [1.0, 0.0],
    flat_chris, t_end=1.0
)
print(f"{V_flat[0]:.4f}")
print(f"{V_flat[1]:.4f}")
`,
  tests: [
    {
      name: "flat R2: V^x stays 1.0",
      code: `{{FUNC}}
flat_chris = christoffel(lambda p: [[1, 0], [0, 1]])
V = parallel_transport([0.0, 0.0], [1.0, 0.0], lambda t, p: [1.0, 0.0], flat_chris, t_end=1.0)
print(f"{V[0]:.4f}")`,
      expected: "1.0000\n",
    },
    {
      name: "flat R2: V^y stays 0.0",
      code: `{{FUNC}}
flat_chris = christoffel(lambda p: [[1, 0], [0, 1]])
V = parallel_transport([0.0, 0.0], [1.0, 0.0], lambda t, p: [1.0, 0.0], flat_chris, t_end=1.0)
print(f"{V[1]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "sphere meridian transport: V^φ → 2.0 at t=π/3",
      code: `{{FUNC}}
import math
sphere_chris = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
# Transport [0,1] from θ=π/2 along meridian (dθ/dt=-1, dφ/dt=0) for t=π/3
V = parallel_transport(
    [math.pi/2, 0.0], [0.0, 1.0],
    lambda t, p: [-1.0, 0.0],
    sphere_chris, t_end=math.pi/3, steps=5000
)
print(f"{V[1]:.4f}")`,
      expected: "2.0000\n",
    },
    {
      name: "sphere meridian transport: V^θ stays 0.0",
      code: `{{FUNC}}
import math
sphere_chris = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
V = parallel_transport(
    [math.pi/2, 0.0], [0.0, 1.0],
    lambda t, p: [-1.0, 0.0],
    sphere_chris, t_end=math.pi/3, steps=5000
)
print(f"{V[0]:.4f}")`,
      expected: "0.0000\n",
    },
  ],
};
