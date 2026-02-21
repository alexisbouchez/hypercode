import type { Lesson } from "../../types";

export const duffingOscillator: Lesson = {
  id: "duffing-oscillator",
  title: "Duffing Oscillator",
  chapterId: "classic-attractors",
  content: `## Duffing Oscillator

The **Duffing oscillator** is a driven nonlinear oscillator modelling a mass in a double-well potential with damping and periodic forcing:

$$\\dot{x} = y, \\quad \\dot{y} = -\\delta y - \\alpha x - \\beta x^3 + \\gamma \\cos(\\omega t)$$

With parameters **δ = 0.3, α = −1, β = 1, γ = 0.5, ω = 1.2**, the system exhibits a rich variety of behaviour: periodic orbits, period-doubling cascades, and fully chaotic attractors depending on the forcing amplitude γ.

The term **−αx − βx³** creates a double-well potential (two stable equilibria). The periodic driving can kick the system from one well to the other chaotically — a model for physical phenomena from buckled beams to certain electronic circuits.

**Implement the following functions:**
- \`duffing_deriv(x, y, t, delta, alpha, beta, gamma, omega)\` — compute the derivatives (ẋ, ẏ) at time t
- \`duffing_euler(x0, y0, delta, alpha, beta, gamma, omega, dt, steps)\` — integrate using Euler's method, advancing t by dt each step, returning final (x, y)`,
  starterCode: `import math

def duffing_deriv(x, y, t, delta, alpha, beta, gamma, omega):
    # Return (dx, dy) for the Duffing oscillator
    pass

def duffing_euler(x0, y0, delta, alpha, beta, gamma, omega, dt, steps):
    # Euler-integrate the Duffing system, return final (x, y)
    pass`,
  solution: `import math

def duffing_deriv(x, y, t, delta, alpha, beta, gamma, omega):
    dx = y
    dy = -delta * y - alpha * x - beta * x**3 + gamma * math.cos(omega * t)
    return dx, dy

def duffing_euler(x0, y0, delta, alpha, beta, gamma, omega, dt, steps):
    x, y, t = x0, y0, 0.0
    for _ in range(steps):
        dx, dy = duffing_deriv(x, y, t, delta, alpha, beta, gamma, omega)
        x += dt * dx
        y += dt * dy
        t += dt
    return x, y`,
  tests: [
    {
      name: "duffing derivative at rest",
      code: `{{FUNC}}\ndx,dy = duffing_deriv(0.0,0.0,0.0,0.3,-1.0,1.0,0.5,1.2)\nprint(round(dx,4), round(dy,4))`,
      expected: "0.0 0.5\n",
    },
    {
      name: "zero steps identity",
      code: `{{FUNC}}\nx,y = duffing_euler(1.0,0.5,0.3,-1.0,1.0,0.5,1.2,0.01,0)\nprint(x,y)`,
      expected: "1.0 0.5\n",
    },
    {
      name: "one step from rest",
      code: `{{FUNC}}\nx,y = duffing_euler(0.0,0.0,0.3,-1.0,1.0,0.5,1.2,0.1,1)\nprint(round(x,4), round(y,4))`,
      expected: "0.0 0.05\n",
    },
  ],
};
