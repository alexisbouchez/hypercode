import type { Lesson } from "../../types";

export const laplaceTransform: Lesson = {
  id: "laplace-transform",
  title: "Laplace Transform",
  chapterId: "transforms",
  content: `# Laplace Transform

The **Laplace transform** converts a time-domain function f(t) into a complex-frequency-domain function F(s):

$$F(s) = \\mathcal{L}\\{f(t)\\} = \\int_0^{\\infty} f(t)\\, e^{-st}\\, dt$$

It is one of the most powerful tools in mathematical physics, turning differential equations into algebraic ones.

## Common Transform Pairs

| f(t) | F(s) |
|------|------|
| 1 | 1/s |
| $e^{at}$ | $1/(s-a)$ |
| $\\sin(\\omega t)$ | $\\omega/(s^2+\\omega^2)$ |
| $\\cos(\\omega t)$ | $s/(s^2+\\omega^2)$ |
| $t^n$ | $n!/s^{n+1}$ |

## The Convolution Theorem

If $F(s) = \\mathcal{L}\\{f\\}$ and $G(s) = \\mathcal{L}\\{g\\}$, then:

$$\\mathcal{L}\\{f * g\\} = F(s)\\cdot G(s)$$

where the convolution is $$(f*g)(t) = \\int_0^t f(\\tau) g(t-\\tau)\\, d\\tau$$

This makes solving linear ODEs with initial conditions straightforward: apply the transform, solve the algebraic equation, then invert.

## Numerical Laplace Transform

For functions without a closed-form transform, we approximate:

$$F(s) \\approx \\sum_{i=0}^{N-1} f(t_i)\\, e^{-s\\, t_i}\\, \\Delta t$$

where $t_i = i \\cdot \\Delta t$ and $\\Delta t = T_{\\max}/N$. This rectangle-rule quadrature works well when f(t) decays fast enough that the tail beyond $T_{\\max}$ is negligible.

## Implementation

Implement the following functions:

- \`laplace_exp(a, s)\` — analytic: returns $1/(s-a)$
- \`laplace_sin(omega, s)\` — analytic: returns $\\omega/(s^2+\\omega^2)$
- \`laplace_cos(omega, s)\` — analytic: returns $s/(s^2+\\omega^2)$
- \`laplace_tn(n, s)\` — analytic: returns $n!/s^{n+1}$
- \`laplace_numerical(f_func, s, T_max=20, N=10000)\` — numerical rectangle-rule approximation

\`\`\`python
import math

def laplace_exp(a, s):
    return 1.0 / (s - a)

def laplace_sin(omega, s):
    return omega / (s**2 + omega**2)
\`\`\`
`,
  starterCode: `import math

def laplace_exp(a, s):
    pass

def laplace_sin(omega, s):
    pass

def laplace_cos(omega, s):
    pass

def laplace_tn(n, s):
    pass

def laplace_numerical(f_func, s, T_max=20, N=10000):
    pass
`,
  solution: `import math

def laplace_exp(a, s):
    return 1.0 / (s - a)

def laplace_sin(omega, s):
    return omega / (s**2 + omega**2)

def laplace_cos(omega, s):
    return s / (s**2 + omega**2)

def laplace_tn(n, s):
    factorial_n = math.factorial(n)
    return factorial_n / s**(n + 1)

def laplace_numerical(f_func, s, T_max=20, N=10000):
    dt = T_max / N
    total = 0.0
    for i in range(N):
        t = i * dt
        total += f_func(t) * math.exp(-s * t) * dt
    return total
`,
  tests: [
    {
      name: "laplace_sin(2.0, 3.0) = omega/(s^2+omega^2) = 2/13",
      expected: "0.1538\n",
      code: `{{FUNC}}\nprint(f"{laplace_sin(2.0, 3.0):.4f}")`,
    },
    {
      name: "laplace_cos(2.0, 3.0) = s/(s^2+omega^2) = 3/13",
      expected: "0.2308\n",
      code: `{{FUNC}}\nprint(f"{laplace_cos(2.0, 3.0):.4f}")`,
    },
    {
      name: "laplace_tn(2, 2.0) = 2!/2^3 = 0.25",
      expected: "0.2500\n",
      code: `{{FUNC}}\nprint(f"{laplace_tn(2, 2.0):.4f}")`,
    },
    {
      name: "laplace_numerical of exp(-t) at s=2 ≈ 1/3",
      expected: "0.3343\n",
      code: `{{FUNC}}\nprint(f"{laplace_numerical(lambda t: math.exp(-t), 2.0):.4f}")`,
    },
  ],
};
