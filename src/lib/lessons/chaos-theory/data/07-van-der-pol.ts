import type { Lesson } from "../../types";

export const vanDerPol: Lesson = {
  id: "van-der-pol",
  title: "Van der Pol Oscillator",
  chapterId: "classic-attractors",
  content: `## Van der Pol Oscillator

The **Van der Pol oscillator** is a nonlinear oscillator with self-sustaining oscillations. It was originally developed to model vacuum tube circuits but appears throughout biology, physics, and engineering.

### The Equations

\`\`\`
dx/dt = y
dy/dt = μ·(1 - x²)·y - x
\`\`\`

The parameter **μ** controls the nonlinearity:
- **μ = 0**: Simple harmonic oscillator (no damping/driving)
- **μ > 0**: System has a **limit cycle** — all trajectories spiral toward a stable periodic orbit
- **Large μ**: Relaxation oscillations with sharp transitions

### Limit Cycles

Unlike fixed-point attractors, the Van der Pol oscillator has a **limit cycle attractor**. Regardless of initial conditions, trajectories converge to the same closed orbit in phase space. This is a hallmark of many biological oscillators (heartbeat, circadian rhythms).

### Euler Integration

We use simple Euler integration with timestep dt:
\`\`\`
x_{n+1} = x_n + dt·(dx/dt)
y_{n+1} = y_n + dt·(dy/dt)
\`\`\`

### Your Task

Implement:
1. \`vdp_deriv(x, y, mu)\` — compute derivatives (dx/dt, dy/dt)
2. \`vdp_euler(x0, y0, mu, dt, steps)\` — integrate using Euler method
3. \`vdp_amplitude(x0, y0, mu, dt, steps, n_keep)\` — find max amplitude after transients`,
  starterCode: `def vdp_deriv(x, y, mu):
    # Return (dx/dt, dy/dt) for Van der Pol
    # dx/dt = y
    # dy/dt = mu*(1 - x^2)*y - x
    pass

def vdp_euler(x0, y0, mu, dt, steps):
    # Integrate using Euler method for given number of steps
    # Return final (x, y)
    pass

def vdp_amplitude(x0, y0, mu, dt, steps, n_keep):
    # Run 'steps' to reach steady state, then track max |x| over 'n_keep' steps
    pass`,
  solution: `def vdp_deriv(x, y, mu):
    dx = y
    dy = mu * (1.0 - x * x) * y - x
    return dx, dy

def vdp_euler(x0, y0, mu, dt, steps):
    x, y = x0, y0
    for _ in range(steps):
        dx, dy = vdp_deriv(x, y, mu)
        x += dt * dx
        y += dt * dy
    return x, y

def vdp_amplitude(x0, y0, mu, dt, steps, n_keep):
    x, y = x0, y0
    for _ in range(steps):
        dx, dy = vdp_deriv(x, y, mu)
        x += dt * dx
        y += dt * dy
    max_x = 0.0
    for _ in range(n_keep):
        dx, dy = vdp_deriv(x, y, mu)
        x += dt * dx
        y += dt * dy
        if abs(x) > max_x:
            max_x = abs(x)
    return max_x`,
  tests: [
    {
      name: "vdp deriv at origin",
      code: `{{FUNC}}\ndx,dy = vdp_deriv(0.0,0.0,1.0)\nprint(dx,dy)`,
      expected: "0.0 0.0\n",
    },
    {
      name: "vdp deriv at (0,1)",
      code: `{{FUNC}}\ndx,dy = vdp_deriv(0.0,1.0,1.0)\nprint(dx,dy)`,
      expected: "1.0 1.0\n",
    },
    {
      name: "zero steps identity",
      code: `{{FUNC}}\nx,y = vdp_euler(1.0,0.0,1.0,0.01,0)\nprint(x,y)`,
      expected: "1.0 0.0\n",
    },
  ],
};
