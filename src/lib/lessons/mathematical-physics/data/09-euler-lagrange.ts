import type { Lesson } from "../../types";

export const eulerLagrange: Lesson = {
  id: "euler-lagrange",
  title: "Euler-Lagrange Equation",
  chapterId: "variational",
  content: `# Euler-Lagrange Equation

The **calculus of variations** asks: which function $y(x)$ extremizes the functional

$$S[y] = \\int_{x_1}^{x_2} L(x, y, y')\\, dx$$

where $L$ is the **Lagrangian density**? The answer is the **Euler-Lagrange equation**:

$$\\frac{d}{dx}\\!\\left(\\frac{\\partial L}{\\partial y'}\\right) - \\frac{\\partial L}{\\partial y} = 0$$

## Classic Examples

**Shortest path:** $L = \\sqrt{1 + y'^2}$. The E-L equation gives $y'' = 0$, i.e., a straight line.

**Brachistochrone:** The curve of fastest descent under gravity is a **cycloid**, found by minimizing the travel time functional.

**Simple pendulum:** The Lagrangian $L = T - V = \\tfrac{1}{2}ml^2\\dot\\theta^2 - mgl(1-\\cos\\theta)$ gives the equation of motion:

$$\\ddot\\theta + \\frac{g}{l}\\sin\\theta = 0$$

In the **small-angle approximation** ($\\sin\\theta \\approx \\theta$), this becomes simple harmonic motion with angular frequency $\\omega = \\sqrt{g/l}$.

## Pendulum Period

The small-angle period is:

$$T = 2\\pi\\sqrt{\\frac{l}{g}}$$

The **small-angle solution** is $\\theta(t) = \\theta_0 \\cos(\\omega t + \\phi)$.

## Numerical Action

Given discrete sample points $(x_i, y_i)$, approximate $S[y]$ numerically:

1. Estimate $y'_i \\approx (y_{i+1} - y_i) / (x_{i+1} - x_i)$ (forward difference).
2. Evaluate $L$ at the midpoint of each interval.
3. Sum: $S \\approx \\sum_i L(x_{\\text{mid}}, y_{\\text{mid}}, y'_i)\\, \\Delta x_i$.

## Your Task

Implement \`action(L_func, y_values, x_values)\` using the midpoint-rectangle rule above. Implement \`pendulum_period_s(l_m, g=9.81)\` for the small-angle period. Implement \`pendulum_angle_rad(t_s, theta0_rad, l_m, g=9.81)\` for the small-angle trajectory $\\theta_0 \\cos(\\omega t)$.

All constants must be computed inside the function bodies.
`,
  starterCode: `import math

def action(L_func, y_values, x_values):
    pass

def pendulum_period_s(l_m, g=9.81):
    pass

def pendulum_angle_rad(t_s, theta0_rad, l_m, g=9.81):
    pass
`,
  solution: `import math

def action(L_func, y_values, x_values):
    total = 0.0
    n = len(x_values)
    for i in range(n - 1):
        dx = x_values[i + 1] - x_values[i]
        yp = (y_values[i + 1] - y_values[i]) / dx
        x_mid = (x_values[i] + x_values[i + 1]) / 2.0
        y_mid = (y_values[i] + y_values[i + 1]) / 2.0
        total += L_func(y_mid, yp, x_mid) * dx
    return total

def pendulum_period_s(l_m, g=9.81):
    return 2.0 * math.pi * math.sqrt(l_m / g)

def pendulum_angle_rad(t_s, theta0_rad, l_m, g=9.81):
    omega = math.sqrt(g / l_m)
    return theta0_rad * math.cos(omega * t_s)
`,
  tests: [
    {
      name: "pendulum period for l=1m",
      expected: "2.0061\n",
      code: `{{FUNC}}
print(f"{pendulum_period_s(1.0):.4f}")`,
    },
    {
      name: "pendulum period for l=0.25m",
      expected: "1.0030\n",
      code: `{{FUNC}}
print(f"{pendulum_period_s(0.25):.4f}")`,
    },
    {
      name: "pendulum angle at t=0.5s",
      expected: "0.0005\n",
      code: `{{FUNC}}
print(f"{pendulum_angle_rad(0.5, 0.1, 1.0):.4f}")`,
    },
    {
      name: "action of y'=1 (straight line, L=y'^2) equals 1",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{action(lambda y, yp, x: yp**2, [0, 0.5, 1.0], [0, 0.5, 1.0]):.4f}")`,
    },
  ],
};
