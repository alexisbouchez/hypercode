import type { Lesson } from "../../types";

export const heatEquation: Lesson = {
  id: "heat-equation",
  title: "Heat Equation",
  chapterId: "pdes",
  content: `# Heat Equation

The **1D heat equation** describes how temperature $u(x, t)$ diffuses through a material:

$$\\frac{\\partial u}{\\partial t} = \\alpha\\, \\frac{\\partial^2 u}{\\partial x^2}$$

where $\\alpha > 0$ is the thermal diffusivity.

## Separation of Variables

For the rod $x \\in [0, L]$ with **Dirichlet boundary conditions** $u(0,t) = u(L,t) = 0$ and initial condition $u(x, 0) = f(x)$, the solution is:

$$u(x, t) = \\sum_{n=1}^{\\infty} B_n \\sin\\!\\left(\\frac{n\\pi x}{L}\\right) \\exp\\!\\left(-\\alpha\\left(\\frac{n\\pi}{L}\\right)^2 t\\right)$$

## Fourier Coefficients

The Fourier sine coefficients $B_n$ are determined by projecting the initial condition onto the sine basis:

$$B_n = \\frac{2}{L} \\int_0^L f(x) \\sin\\!\\left(\\frac{n\\pi x}{L}\\right) dx$$

Each mode $n$ decays exponentially in time with rate $\\alpha(n\\pi/L)^2$. High-frequency modes decay fastest — this is why heat smooths out sharp features rapidly.

## Numerical Integration

Given $f$ sampled on a uniform grid of $N_{\\text{grid}}$ interior points $x_k = k \\cdot L / N_{\\text{grid}}$ for $k = 1, \\ldots, N_{\\text{grid}} - 1$, approximate the integral with the rectangle rule:

$$B_n \\approx \\frac{2}{L} \\sum_{k=1}^{N_{\\text{grid}}-1} f(x_k) \\sin\\!\\left(\\frac{n\\pi x_k}{L}\\right) \\Delta x$$

where $\\Delta x = L / N_{\\text{grid}}$.

## Example

For $f(x) = \\sin(\\pi x / L)$, only $B_1 = 1$ is nonzero (all other $B_n = 0$), giving:

$$u(x, t) = \\sin\\!\\left(\\frac{\\pi x}{L}\\right) e^{-\\alpha(\\pi/L)^2 t}$$

The temperature profile maintains its sinusoidal shape but decays uniformly.

## Your Task

Implement \`heat_coefficient_Bn(f_values, L, n, N_grid=100)\` where \`f_values\` is a list of $N_{\\text{grid}}-1$ interior sample values. Implement \`heat_solution(x, t, alpha, L, Bn_list)\` that evaluates the series given a precomputed list $[B_1, B_2, \\ldots]$. Implement \`heat_series(x, t, alpha, L, f_values, N_terms=10)\` that computes coefficients and evaluates the solution in one call.
`,
  starterCode: `import math

def heat_coefficient_Bn(f_values, L, n, N_grid=100):
    pass

def heat_solution(x, t, alpha, L, Bn_list):
    pass

def heat_series(x, t, alpha, L, f_values, N_terms=10):
    pass
`,
  solution: `import math

def heat_coefficient_Bn(f_values, L, n, N_grid=100):
    dx = L / N_grid
    total = 0.0
    for i, fval in enumerate(f_values):
        x = (i + 1) * dx
        total += fval * math.sin(n * math.pi * x / L) * dx
    return (2.0 / L) * total

def heat_solution(x, t, alpha, L, Bn_list):
    result = 0.0
    for i, Bn in enumerate(Bn_list):
        n = i + 1
        result += Bn * math.sin(n * math.pi * x / L) * math.exp(-alpha * (n * math.pi / L)**2 * t)
    return result

def heat_series(x, t, alpha, L, f_values, N_terms=10):
    Bn_list = [heat_coefficient_Bn(f_values, L, n) for n in range(1, N_terms + 1)]
    return heat_solution(x, t, alpha, L, Bn_list)
`,
  tests: [
    {
      name: "Bn for sin(pi*x/L) initial condition",
      expected: "1.0000\n",
      code: `{{FUNC}}
import math
f_vals = [math.sin(math.pi * k / 100) for k in range(1, 100)]
print(f"{heat_coefficient_Bn(f_vals, 1.0, 1):.4f}")`,
    },
    {
      name: "heat_solution at x=0.5, t=0.1, alpha=1",
      expected: "0.3727\n",
      code: `{{FUNC}}
print(f"{heat_solution(0.5, 0.1, 1.0, 1.0, [1.0]):.4f}")`,
    },
    {
      name: "heat_solution at t=0 equals initial condition",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{heat_solution(0.5, 0.0, 1.0, 1.0, [1.0]):.4f}")`,
    },
    {
      name: "heat_solution at x=0.25, t=0.1",
      expected: "0.2635\n",
      code: `{{FUNC}}
print(f"{heat_solution(0.25, 0.1, 1.0, 1.0, [1.0]):.4f}")`,
    },
  ],
};
