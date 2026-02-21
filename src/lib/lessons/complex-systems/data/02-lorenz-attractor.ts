import type { Lesson } from "../../types";

export const lorenzAttractor: Lesson = {
  id: "lorenz-attractor",
  title: "Lorenz Attractor",
  chapterId: "dynamical-systems",
  content: `# Lorenz Attractor

In 1963, Edward Lorenz discovered that a simplified model of atmospheric convection produces strikingly complex, non-repeating trajectories — the first famous example of a **strange attractor**.

## The Lorenz System

$$\\frac{dx}{dt} = \\sigma(y - x)$$
$$\\frac{dy}{dt} = x(\\rho - z) - y$$
$$\\frac{dz}{dt} = xy - \\beta z$$

**Classic parameters:** $\\sigma = 10$, $\\rho = 28$, $\\beta = 8/3$

The state vector $(x, y, z)$ evolves continuously. With these parameters the system is chaotic, with a Lyapunov exponent of approximately $0.9$.

## Numerical Integration: RK4

We integrate using the **fourth-order Runge-Kutta** (RK4) method with time step $dt = 0.01$:

$$k_1 = f(s) \\quad k_2 = f\\!\\left(s + \\tfrac{dt}{2}k_1\\right) \\quad k_3 = f\\!\\left(s + \\tfrac{dt}{2}k_2\\right) \\quad k_4 = f(s + dt\\,k_3)$$

$$s_{n+1} = s_n + \\frac{dt}{6}(k_1 + 2k_2 + 2k_3 + k_4)$$

## Key Properties

- **Sensitivity to initial conditions:** two trajectories starting $10^{-10}$ apart diverge exponentially.
- **Strange attractor:** trajectories are bounded but never repeat — they lie on a fractal set with dimension $\\approx 2.06$.
- **Butterfly shape:** the attractor has two lobes around the two unstable fixed points at $(\\pm\\sqrt{\\beta(\\rho-1)}, \\pm\\sqrt{\\beta(\\rho-1)}, \\rho-1)$.

## Your Task

Implement:
- \`lorenz_deriv(state, sigma=10, rho=28, beta=8/3)\` — returns $(dx, dy, dz)$ as a tuple
- \`lorenz_rk4_step(state, sigma=10, rho=28, beta=8/3, dt=0.01)\` — one RK4 step, returns new $(x, y, z)$
- \`lorenz_trajectory(x0, y0, z0, n_steps=1000, dt=0.01)\` — returns list of $(x,y,z)$ tuples (including initial state)
`,
  starterCode: `def lorenz_deriv(state, sigma=10, rho=28, beta=None):
    if beta is None:
        beta = 8.0 / 3.0
    pass

def lorenz_rk4_step(state, sigma=10, rho=28, beta=None, dt=0.01):
    if beta is None:
        beta = 8.0 / 3.0
    pass

def lorenz_trajectory(x0, y0, z0, n_steps=1000, dt=0.01):
    pass
`,
  solution: `def lorenz_deriv(state, sigma=10, rho=28, beta=None):
    if beta is None:
        beta = 8.0 / 3.0
    x, y, z = state
    dx = sigma * (y - x)
    dy = x * (rho - z) - y
    dz = x * y - beta * z
    return (dx, dy, dz)

def lorenz_rk4_step(state, sigma=10, rho=28, beta=None, dt=0.01):
    if beta is None:
        beta = 8.0 / 3.0
    x, y, z = state
    k1 = lorenz_deriv(state, sigma, rho, beta)
    k2 = lorenz_deriv((x + 0.5*dt*k1[0], y + 0.5*dt*k1[1], z + 0.5*dt*k1[2]), sigma, rho, beta)
    k3 = lorenz_deriv((x + 0.5*dt*k2[0], y + 0.5*dt*k2[1], z + 0.5*dt*k2[2]), sigma, rho, beta)
    k4 = lorenz_deriv((x + dt*k3[0], y + dt*k3[1], z + dt*k3[2]), sigma, rho, beta)
    nx = x + dt / 6 * (k1[0] + 2*k2[0] + 2*k3[0] + k4[0])
    ny = y + dt / 6 * (k1[1] + 2*k2[1] + 2*k3[1] + k4[1])
    nz = z + dt / 6 * (k1[2] + 2*k2[2] + 2*k3[2] + k4[2])
    return (nx, ny, nz)

def lorenz_trajectory(x0, y0, z0, n_steps=1000, dt=0.01):
    state = (x0, y0, z0)
    traj = [state]
    for _ in range(n_steps):
        state = lorenz_rk4_step(state, dt=dt)
        traj.append(state)
    return traj
`,
  tests: [
    {
      name: "lorenz_deriv((1,1,1))[0] == 0 (no x-drive when x==y)",
      expected: "0.0000\n",
      code: `{{FUNC}}
print(f"{lorenz_deriv((1,1,1))[0]:.4f}")`,
    },
    {
      name: "lorenz_deriv((0,1,0))[0] == 10 (full sigma drive)",
      expected: "10.0000\n",
      code: `{{FUNC}}
print(f"{lorenz_deriv((0,1,0))[0]:.4f}")`,
    },
    {
      name: "lorenz_rk4_step((1,1,1))[0] after one step",
      expected: "1.0126\n",
      code: `{{FUNC}}
print(f"{lorenz_rk4_step((1,1,1))[0]:.4f}")`,
    },
    {
      name: "lorenz_trajectory(0,1,0,100)[-1][2] z-component after 100 steps",
      expected: "28.3378\n",
      code: `{{FUNC}}
print(f"{lorenz_trajectory(0,1,0,100)[-1][2]:.4f}")`,
    },
  ],
};
