import type { Lesson } from "../../types";

export const geodesics: Lesson = {
  id: "geodesics",
  title: "Geodesic Equations",
  chapterId: "connections-curvature",
  content: `# Geodesic Equations

**Geodesics** are the "straightest possible curves" on a manifold — the generalization of straight lines to curved spaces. A curve $\\gamma(t)$ is a geodesic if its velocity vector is parallel-transported along itself:

$$\\nabla_{\\dot\\gamma} \\dot\\gamma = 0$$

In coordinates, this becomes the geodesic equation:

$$\\frac{d^2 x^k}{dt^2} + \\Gamma^k_{ij}\\frac{dx^i}{dt}\\frac{dx^j}{dt} = 0$$

## Connection to the Lagrangian

This is exactly the Euler-Lagrange equation for the Lagrangian $L = \\frac{1}{2}g_{ij}\\dot{x}^i\\dot{x}^j$! The Christoffel coefficients appear in the Lagrange equations for free motion — the deep connection between geometry and dynamics.

## Integration

Introduce the velocity $v^k = dx^k/dt$ as an auxiliary variable:

$$\\frac{dx^k}{dt} = v^k, \\qquad \\frac{dv^k}{dt} = -\\Gamma^k_{ij}\\, v^i\\, v^j$$

This is a first-order ODE system we can integrate with RK4.

## Examples

**Flat $\\mathbb{R}^2$**: All $\\Gamma = 0$, so $\\ddot{x} = 0$: straight lines.

**Unit sphere**: Geodesics are **great circles** — the equator, meridians, and any circle obtained by slicing the sphere with a plane through the center.

The equator $\\gamma(t) = (\\pi/2, t)$ is a geodesic: at $\\theta = \\pi/2$, $\\Gamma^\\theta_{\\phi\\phi} = -\\sin(\\pi/2)\\cos(\\pi/2) = 0$, so no acceleration.

## Your Task

Implement \`integrate_geodesic(p0, v0, christoffel_fn, t_end, steps)\` using RK4 integration. Return the final position.
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

def integrate_geodesic(p0, v0, christoffel_fn, t_end, steps=500):
    """Integrate geodesic equations using RK4.
    State = p + v (concatenated).
    Returns final position p.
    """
    pass

# Flat R2: straight line from (0,0) with velocity (1,0)
flat_chris = christoffel(lambda p: [[1, 0], [0, 1]])
p_final = integrate_geodesic([0.0, 0.0], [1.0, 0.0], flat_chris, t_end=1.0)
print(f"{p_final[0]:.4f}")  # 1.0000
print(f"{p_final[1]:.4f}")  # 0.0000
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

def integrate_geodesic(p0, v0, christoffel_fn, t_end, steps=500):
    n = len(p0)
    p = list(p0)
    v = list(v0)
    dt = t_end / steps

    def deriv(p, v):
        Gamma = christoffel_fn(p)
        accel = [-sum(Gamma[k][i][j] * v[i] * v[j]
                      for i in range(n) for j in range(n))
                 for k in range(n)]
        return v, accel

    for _ in range(steps):
        v1, a1 = deriv(p, v)
        p2 = [p[k] + 0.5*dt*v1[k] for k in range(n)]
        v2_s = [v[k] + 0.5*dt*a1[k] for k in range(n)]
        v2, a2 = deriv(p2, v2_s)
        p3 = [p[k] + 0.5*dt*v2[k] for k in range(n)]
        v3_s = [v[k] + 0.5*dt*a2[k] for k in range(n)]
        v3, a3 = deriv(p3, v3_s)
        p4 = [p[k] + dt*v3[k] for k in range(n)]
        v4_s = [v[k] + dt*a3[k] for k in range(n)]
        v4, a4 = deriv(p4, v4_s)
        p = [p[k] + (dt/6)*(v1[k]+2*v2[k]+2*v3[k]+v4[k]) for k in range(n)]
        v = [v[k] + (dt/6)*(a1[k]+2*a2[k]+2*a3[k]+a4[k]) for k in range(n)]

    return p

flat_chris = christoffel(lambda p: [[1, 0], [0, 1]])
p_final = integrate_geodesic([0.0, 0.0], [1.0, 0.0], flat_chris, t_end=1.0)
print(f"{p_final[0]:.4f}")
print(f"{p_final[1]:.4f}")
`,
  tests: [
    {
      name: "flat R2: x moves to 1.0",
      code: `{{FUNC}}
flat = christoffel(lambda p: [[1, 0], [0, 1]])
p = integrate_geodesic([0.0, 0.0], [1.0, 0.0], flat, t_end=1.0)
print(f"{p[0]:.4f}")`,
      expected: "1.0000\n",
    },
    {
      name: "flat R2: y stays 0.0",
      code: `{{FUNC}}
flat = christoffel(lambda p: [[1, 0], [0, 1]])
p = integrate_geodesic([0.0, 0.0], [1.0, 0.0], flat, t_end=1.0)
print(f"{p[1]:.4f}")`,
      expected: "0.0000\n",
    },
    {
      name: "sphere equator geodesic: θ stays π/2",
      code: `{{FUNC}}
import math
sphere = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
p = integrate_geodesic([math.pi/2, 0.0], [0.0, 1.0], sphere, t_end=math.pi/2, steps=1000)
print(f"{p[0]:.4f}")`,
      expected: "1.5708\n",
    },
    {
      name: "sphere equator geodesic: φ reaches π/2",
      code: `{{FUNC}}
import math
sphere = christoffel(lambda p: [[1, 0], [0, math.sin(p[0])**2]])
p = integrate_geodesic([math.pi/2, 0.0], [0.0, 1.0], sphere, t_end=math.pi/2, steps=1000)
print(f"{p[1]:.4f}")`,
      expected: "1.5708\n",
    },
  ],
};
