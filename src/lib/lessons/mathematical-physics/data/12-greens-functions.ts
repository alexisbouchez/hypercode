import type { Lesson } from "../../types";

export const greensFunctions: Lesson = {
  id: "greens-functions",
  title: "Green's Functions",
  chapterId: "pdes",
  content: `# Green's Functions

A **Green's function** is the impulse response of a differential operator — the solution when the source term is a Dirac delta. If $\\mathcal{L}$ is a linear differential operator, then:

$$\\mathcal{L}\\, G(\\mathbf{r}, \\mathbf{r}') = \\delta(\\mathbf{r} - \\mathbf{r}')$$

Once we have G, the solution to $\\mathcal{L}\\, u = f$ is:

$$u(\\mathbf{r}) = \\int G(\\mathbf{r}, \\mathbf{r}')\\, f(\\mathbf{r}')\\, d\\mathbf{r}'$$

## 1D Poisson Equation

For $-d^2u/dx^2 = f(x)$ on $[0, L]$ with $u(0) = u(L) = 0$:

$$G(x, x') = \\frac{1}{L} \\min(x, x')\\bigl(L - \\max(x, x')\\bigr)$$

This has a characteristic "tent" shape — it is continuous with a kink at $x = x'$.

The solution is then:

$$u(x) = \\int_0^L G(x, x')\\, f(x')\\, dx'$$

For example, if $f(x) = 1$ (constant forcing), the solution is $u(x) = x(L-x)/2$ — a parabolic profile that satisfies the boundary conditions and the equation.

## Free-Space Green's Function (1D Laplacian)

In 1D free space (no boundary conditions), the Green's function for $-d^2/dx^2$ is:

$$G(x, x') = -\\frac{|x - x'|}{2}$$

## Numerical Solution via Green's Function

We discretise $x'$ into $N$ points and approximate the integral as a sum:

$$u(x) \\approx \\sum_{i=0}^{N-1} G(x, x_i')\\, f(x_i')\\, \\Delta x'$$

where $x_i' = (i + 0.5) \\cdot L/N$ and $\\Delta x' = L/N$.

## Implementation

- \`greens_1d_poisson(x, x_prime, L)\` — returns $G(x, x')$ for the 1D Poisson problem
- \`greens_free_space_1d(x, x_prime)\` — returns $-|x - x'|/2$
- \`poisson_solution(x, f_values, L, N=100)\` — evaluates $u(x)$ by summing $G \\cdot f \\cdot \\Delta x$

\`\`\`python
import math

def greens_1d_poisson(x, x_prime, L):
    return (1.0 / L) * min(x, x_prime) * (L - max(x, x_prime))
\`\`\`
`,
  starterCode: `import math

def greens_1d_poisson(x, x_prime, L):
    pass

def greens_free_space_1d(x, x_prime):
    pass

def poisson_solution(x, f_values, L, N=100):
    pass
`,
  solution: `import math

def greens_1d_poisson(x, x_prime, L):
    return (1.0 / L) * min(x, x_prime) * (L - max(x, x_prime))

def greens_free_space_1d(x, x_prime):
    return -abs(x - x_prime) / 2.0

def poisson_solution(x, f_values, L, N=100):
    dx = L / N
    result = 0.0
    for i in range(N):
        x_prime = (i + 0.5) * dx
        result += greens_1d_poisson(x, x_prime, L) * f_values[i] * dx
    return result
`,
  tests: [
    {
      name: "greens_1d_poisson(0.3, 0.7, 1.0) = 0.3 * 0.3 = 0.09",
      expected: "0.0900\n",
      code: `{{FUNC}}\nprint(f"{greens_1d_poisson(0.3, 0.7, 1.0):.4f}")`,
    },
    {
      name: "greens_1d_poisson(0.5, 0.5, 1.0) = 0.5 * 0.5 = 0.25",
      expected: "0.2500\n",
      code: `{{FUNC}}\nprint(f"{greens_1d_poisson(0.5, 0.5, 1.0):.4f}")`,
    },
    {
      name: "greens_free_space_1d(1.0, 3.0) = -|1-3|/2 = -1.0",
      expected: "-1.0000\n",
      code: `{{FUNC}}\nprint(f"{greens_free_space_1d(1.0, 3.0):.4f}")`,
    },
    {
      name: "poisson_solution with f=1 gives u(0.5) = 0.5*0.5/2 = 0.125",
      expected: "0.1250\n",
      code: `{{FUNC}}\nprint(f"{poisson_solution(0.5, [1.0]*100, 1.0):.4f}")`,
    },
  ],
};
