import type { Lesson } from "../../types";

export const legendre: Lesson = {
  id: "legendre",
  title: "Legendre Polynomials",
  chapterId: "special-functions",
  content: `# Legendre Polynomials

**Legendre polynomials** $P_n(x)$ are solutions to Legendre's differential equation and arise naturally in problems with spherical symmetry, such as solving the Laplace equation in spherical coordinates.

## Three-Term Recurrence

The polynomials can be generated efficiently using the recurrence relation:

$$(n+1) P_{n+1}(x) = (2n+1)\\, x\\, P_n(x) - n\\, P_{n-1}(x)$$

with initial conditions $P_0(x) = 1$ and $P_1(x) = x$.

The first few polynomials:
- $P_0(x) = 1$
- $P_1(x) = x$
- $P_2(x) = \\frac{1}{2}(3x^2 - 1)$
- $P_3(x) = \\frac{1}{2}(5x^3 - 3x)$
- $P_4(x) = \\frac{1}{8}(35x^4 - 30x^2 + 3)$

## Orthogonality

Legendre polynomials are orthogonal on $[-1, 1]$:

$$\\int_{-1}^{1} P_m(x)\\, P_n(x)\\, dx = \\frac{2}{2n+1}\\, \\delta_{mn}$$

The normalization constant $\\|P_n\\|^2 = 2/(2n+1)$ is returned by \`legendre_norm(n)\`.

## Rodrigues' Formula

An explicit formula using derivatives:

$$P_n(x) = \\frac{1}{2^n n!} \\frac{d^n}{dx^n} (x^2 - 1)^n$$

## Legendre Series

Any sufficiently smooth function on $[-1, 1]$ can be expanded in a **Legendre series**:

$$f(x) \\approx \\sum_{n=0}^{N} c_n P_n(x)$$

The coefficients are $c_n = \\frac{2n+1}{2} \\int_{-1}^{1} f(x) P_n(x)\\, dx$.

## Your Task

Implement the three functions below. Use only Python's \`math\` module and built-in functions.
`,
  starterCode: `import math

def legendre_P(n, x):
    pass

def legendre_norm(n):
    pass

def legendre_series_eval(coeffs, x):
    pass
`,
  solution: `import math

def legendre_P(n, x):
    if n == 0:
        return 1.0
    if n == 1:
        return float(x)
    p_prev = 1.0
    p_curr = float(x)
    for k in range(1, n):
        p_next = ((2 * k + 1) * x * p_curr - k * p_prev) / (k + 1)
        p_prev = p_curr
        p_curr = p_next
    return p_curr

def legendre_norm(n):
    return 2.0 / (2 * n + 1)

def legendre_series_eval(coeffs, x):
    return sum(coeffs[n] * legendre_P(n, x) for n in range(len(coeffs)))
`,
  tests: [
    {
      name: "P_2(0.5)",
      expected: "-0.1250\n",
      code: `{{FUNC}}
print(f"{legendre_P(2, 0.5):.4f}")`,
    },
    {
      name: "P_3(0.5)",
      expected: "-0.4375\n",
      code: `{{FUNC}}
print(f"{legendre_P(3, 0.5):.4f}")`,
    },
    {
      name: "P_4(0.0)",
      expected: "0.3750\n",
      code: `{{FUNC}}
print(f"{legendre_P(4, 0.0):.4f}")`,
    },
    {
      name: "Legendre series [1, 0, 2] at x=0.5",
      expected: "0.7500\n",
      code: `{{FUNC}}
print(f"{legendre_series_eval([1, 0, 2], 0.5):.4f}")`,
    },
  ],
};
