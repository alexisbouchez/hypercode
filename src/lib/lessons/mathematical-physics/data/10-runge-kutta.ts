import type { Lesson } from "../../types";

export const rungeKutta: Lesson = {
  id: "runge-kutta",
  title: "Runge-Kutta Method",
  chapterId: "numerical",
  content: `# Runge-Kutta 4th Order Method

The **Runge-Kutta 4th order method (RK4)** is the workhorse numerical integrator for ordinary differential equations $dy/dx = f(x, y)$. It achieves $O(h^4)$ global accuracy by sampling the slope at four points per step.

## RK4 Algorithm

Given the current state $(x_n, y_n)$ and step size $h$:

$$k_1 = h\\, f(x_n,\\, y_n)$$
$$k_2 = h\\, f\\!\\left(x_n + \\tfrac{h}{2},\\, y_n + \\tfrac{k_1}{2}\\right)$$
$$k_3 = h\\, f\\!\\left(x_n + \\tfrac{h}{2},\\, y_n + \\tfrac{k_2}{2}\\right)$$
$$k_4 = h\\, f(x_n + h,\\, y_n + k_3)$$

$$y_{n+1} = y_n + \\frac{k_1 + 2k_2 + 2k_3 + k_4}{6}$$

The weighted average gives RK4 its 4th-order accuracy: inner slopes are counted twice.

## Harmonic Oscillator System

The harmonic oscillator $\\ddot{y} + \\omega^2 y = 0$ can be rewritten as a **system of two first-order ODEs**:

$$\\frac{dy}{dt} = v, \\qquad \\frac{dv}{dt} = -\\omega^2 y$$

RK4 is applied to both equations simultaneously at each step. With initial conditions $y(0) = 1$, $v(0) = 0$, the exact solution is $y(t) = \\cos(\\omega t)$.

## Accuracy vs. Step Size

| Step size $h$ | Global error for $dy/dx = y$ |
|--------------|-------------------------------|
| 0.1 | $\\sim 10^{-7}$ |
| 0.01 | $\\sim 10^{-11}$ |

## Your Task

Implement \`rk4_step(f, x, y, h)\` that advances the solution by one step and returns the new $y$ value. Implement \`rk4_solve(f, x0, y0, x_end, h)\` that returns a list of \`(x, y)\` tuples for the full solution. Implement \`harmonic_oscillator_rk4(omega, t_end, y0=1.0, v0=0.0, h=0.01)\` that solves the harmonic oscillator system and returns \`(t_values, y_values)\` as lists.
`,
  starterCode: `import math

def rk4_step(f, x, y, h):
    pass

def rk4_solve(f, x0, y0, x_end, h):
    pass

def harmonic_oscillator_rk4(omega, t_end, y0=1.0, v0=0.0, h=0.01):
    pass
`,
  solution: `import math

def rk4_step(f, x, y, h):
    k1 = h * f(x, y)
    k2 = h * f(x + h / 2.0, y + k1 / 2.0)
    k3 = h * f(x + h / 2.0, y + k2 / 2.0)
    k4 = h * f(x + h, y + k3)
    return y + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0

def rk4_solve(f, x0, y0, x_end, h):
    results = [(x0, y0)]
    x, y = x0, y0
    while x < x_end - 1e-12:
        y = rk4_step(f, x, y, h)
        x += h
        results.append((x, y))
    return results

def harmonic_oscillator_rk4(omega, t_end, y0=1.0, v0=0.0, h=0.01):
    t, y, v = 0.0, y0, v0
    t_vals = [t]
    y_vals = [y]
    while t < t_end - 1e-12:
        k1y = h * v
        k1v = h * (-omega**2 * y)
        k2y = h * (v + k1v / 2.0)
        k2v = h * (-omega**2 * (y + k1y / 2.0))
        k3y = h * (v + k2v / 2.0)
        k3v = h * (-omega**2 * (y + k2y / 2.0))
        k4y = h * (v + k3v)
        k4v = h * (-omega**2 * (y + k3y))
        y += (k1y + 2.0 * k2y + 2.0 * k3y + k4y) / 6.0
        v += (k1v + 2.0 * k2v + 2.0 * k3v + k4v) / 6.0
        t += h
        t_vals.append(t)
        y_vals.append(y)
    return (t_vals, y_vals)
`,
  tests: [
    {
      name: "rk4_step matches e^0.1 for dy/dx=y",
      expected: "1.1052\n",
      code: `{{FUNC}}
print(f"{rk4_step(lambda x, y: y, 0, 1, 0.1):.4f}")`,
    },
    {
      name: "rk4_solve matches e^1 for dy/dx=y",
      expected: "2.7183\n",
      code: `{{FUNC}}
sol = rk4_solve(lambda x, y: y, 0, 1, 1, 0.01)
print(f"{sol[-1][1]:.4f}")`,
    },
    {
      name: "harmonic oscillator at t=pi gives cos(pi)=-1",
      expected: "-1.0000\n",
      code: `{{FUNC}}
import math
t_vals, y_vals = harmonic_oscillator_rk4(1.0, math.pi)
print(f"{y_vals[-1]:.4f}")`,
    },
    {
      name: "harmonic oscillator omega=2 at t=pi gives cos(2pi)≈0.9999",
      expected: "0.9999\n",
      code: `{{FUNC}}
import math
t_vals, y_vals = harmonic_oscillator_rk4(2.0, math.pi)
print(f"{y_vals[-1]:.4f}")`,
    },
  ],
};
