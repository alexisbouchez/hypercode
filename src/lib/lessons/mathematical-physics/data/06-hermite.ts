import type { Lesson } from "../../types";

export const hermite: Lesson = {
  id: "hermite",
  title: "Hermite Polynomials",
  chapterId: "special-functions",
  content: `# Hermite Polynomials

**Hermite polynomials** (physicists' convention) $H_n(x)$ appear as the eigenfunctions of the quantum harmonic oscillator. They satisfy the differential equation:

$$H_n''(x) - 2x\\, H_n'(x) + 2n\\, H_n(x) = 0$$

## Three-Term Recurrence

The most efficient way to evaluate $H_n(x)$ is via the recurrence relation:

$$H_0(x) = 1, \\quad H_1(x) = 2x$$

$$H_{n+1}(x) = 2x\\, H_n(x) - 2n\\, H_{n-1}(x)$$

The first few polynomials are:

| $n$ | $H_n(x)$ |
|-----|----------|
| 0 | $1$ |
| 1 | $2x$ |
| 2 | $4x^2 - 2$ |
| 3 | $8x^3 - 12x$ |
| 4 | $16x^4 - 48x^2 + 12$ |

## Quantum Harmonic Oscillator

The normalized energy eigenstates of the quantum harmonic oscillator (in dimensionless units) are:

$$\\psi_n(x) = \\frac{1}{\\sqrt{2^n\\, n!\\, \\sqrt{\\pi}}}\\, H_n(x)\\, e^{-x^2/2}$$

These satisfy $\\int_{-\\infty}^{\\infty} |\\psi_n(x)|^2\\, dx = 1$.

The probability density of finding the particle at position $x$ in state $n$ is:

$$P_n(x) = |\\psi_n(x)|^2 = \\frac{H_n(x)^2\\, e^{-x^2}}{2^n\\, n!\\, \\sqrt{\\pi}}$$

## Your Task

Implement \`hermite_H(n, x)\` using the three-term recurrence. Implement \`qho_wavefunction(n, x)\` for the normalized wavefunction $\\psi_n(x)$. Implement \`qho_probability(n, x)\` for the probability density $|\\psi_n(x)|^2$.

All constants (factorials, normalization, etc.) must be computed inside the function bodies.
`,
  starterCode: `import math

def hermite_H(n, x):
    pass

def qho_wavefunction(n, x):
    pass

def qho_probability(n, x):
    pass
`,
  solution: `import math

def hermite_H(n, x):
    if n == 0:
        return 1.0
    elif n == 1:
        return 2.0 * x
    h_prev2 = 1.0
    h_prev1 = 2.0 * x
    for k in range(1, n):
        h = 2.0 * x * h_prev1 - 2.0 * k * h_prev2
        h_prev2 = h_prev1
        h_prev1 = h
    return h_prev1

def qho_wavefunction(n, x):
    norm = 1.0 / math.sqrt(2**n * math.factorial(n) * math.sqrt(math.pi))
    return norm * hermite_H(n, x) * math.exp(-x**2 / 2.0)

def qho_probability(n, x):
    psi = qho_wavefunction(n, x)
    return psi ** 2
`,
  tests: [
    {
      name: "hermite_H(0, 1.0) = 1",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{hermite_H(0, 1.0):.4f}")`,
    },
    {
      name: "hermite_H(2, 1.0) = 2",
      expected: "2.0000\n",
      code: `{{FUNC}}
print(f"{hermite_H(2, 1.0):.4f}")`,
    },
    {
      name: "hermite_H(3, 0.5) = -5",
      expected: "-5.0000\n",
      code: `{{FUNC}}
print(f"{hermite_H(3, 0.5):.4f}")`,
    },
    {
      name: "qho_wavefunction(0, 0.0) = pi^(-1/4)",
      expected: "0.7511\n",
      code: `{{FUNC}}
print(f"{qho_wavefunction(0, 0.0):.4f}")`,
    },
  ],
};
