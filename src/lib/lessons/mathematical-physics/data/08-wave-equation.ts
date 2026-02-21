import type { Lesson } from "../../types";

export const waveEquation: Lesson = {
  id: "wave-equation",
  title: "Wave Equation",
  chapterId: "pdes",
  content: `# Wave Equation

The **1D wave equation** governs the propagation of waves (sound, light, strings):

$$\\frac{\\partial^2 u}{\\partial t^2} = c^2\\, \\frac{\\partial^2 u}{\\partial x^2}$$

where $c$ is the wave speed.

## d'Alembert's Solution

For an **infinite domain** with initial displacement $f(x)$ and initial velocity $g(x)$, the general solution is:

$$u(x, t) = \\frac{f(x - ct) + f(x + ct)}{2} + \\frac{1}{2c} \\int_{x-ct}^{x+ct} g(s)\\, ds$$

This has a beautiful physical interpretation: the wave splits into a right-traveling copy and a left-traveling copy of the initial displacement, each at speed $c$.

For zero initial velocity ($g = 0$):

$$u(x, t) = \\frac{f(x - ct) + f(x + ct)}{2}$$

## Standing Waves

On a finite domain $[0, L]$ with Dirichlet boundary conditions, the normal modes are **standing waves**:

$$u_n(x, t) = \\sin\\!\\left(\\frac{n\\pi x}{L}\\right) \\cos\\!\\left(\\frac{n\\pi c t}{L}\\right)$$

The angular frequencies are $\\omega_n = n\\pi c / L$, giving the harmonic series of the string.

## Dispersion Relation

For a non-dispersive medium, the **dispersion relation** is:

$$\\omega = c\\, k$$

The **phase velocity** (speed of a wave crest) equals the **group velocity** (speed of energy transport):

$$v_{\\text{phase}} = \\frac{\\omega}{k} = c$$

## Your Task

Implement \`dalembert(x, t, c, f_func, g_func=None)\` using d'Alembert's formula. If \`g_func\` is provided, integrate numerically using 100 midpoint steps; if \`g_func=None\`, use zero initial velocity. Implement \`standing_wave(x, t, c, L, n=1)\` for the $n$-th normal mode. Implement \`wave_phase_velocity(omega, k)\` returning $\\omega/k$.
`,
  starterCode: `import math

def dalembert(x, t, c, f_func, g_func=None):
    pass

def standing_wave(x, t, c, L, n=1):
    pass

def wave_phase_velocity(omega, k):
    pass
`,
  solution: `import math

def dalembert(x, t, c, f_func, g_func=None):
    result = (f_func(x - c * t) + f_func(x + c * t)) / 2.0
    if g_func is not None:
        a = x - c * t
        b = x + c * t
        n_steps = 100
        h = (b - a) / n_steps
        integral = 0.0
        for i in range(n_steps):
            s = a + (i + 0.5) * h
            integral += g_func(s) * h
        result += integral / (2.0 * c)
    return result

def standing_wave(x, t, c, L, n=1):
    return math.sin(n * math.pi * x / L) * math.cos(n * math.pi * c * t / L)

def wave_phase_velocity(omega, k):
    return omega / k
`,
  tests: [
    {
      name: "standing_wave at t=0 is pure spatial mode",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{standing_wave(0.5, 0.0, 1.0, 1.0):.4f}")`,
    },
    {
      name: "standing_wave at quarter period is zero",
      expected: "0.0000\n",
      code: `{{FUNC}}
import math
print(f"{standing_wave(0.5, 0.5, 1.0, 1.0):.4f}")`,
    },
    {
      name: "d'Alembert with Gaussian initial displacement",
      expected: "0.1840\n",
      code: `{{FUNC}}
import math
print(f"{dalembert(2.0, 1.0, 1.0, lambda x: math.exp(-x**2)):.4f}")`,
    },
    {
      name: "wave phase velocity omega/k",
      expected: "5.0000\n",
      code: `{{FUNC}}
print(f"{wave_phase_velocity(10.0, 2.0):.4f}")`,
    },
  ],
};
