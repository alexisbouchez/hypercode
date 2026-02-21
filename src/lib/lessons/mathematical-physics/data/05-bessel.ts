import type { Lesson } from "../../types";

export const bessel: Lesson = {
  id: "bessel",
  title: "Bessel Functions",
  chapterId: "special-functions",
  content: `# Bessel Functions

**Bessel functions** $J_n(x)$ are solutions to Bessel's differential equation:

$$x^2 y'' + x y' + (x^2 - n^2) y = 0$$

They arise in problems with cylindrical symmetry: heat conduction in cylinders, electromagnetic waveguides, and quantum mechanics in cylindrical potentials.

## Series Definition

The Bessel function of the first kind of order $n$ is defined by the convergent series:

$$J_n(x) = \\sum_{m=0}^{\\infty} \\frac{(-1)^m}{m!\\, \\Gamma(m+n+1)} \\left(\\frac{x}{2}\\right)^{2m+n}$$

For integer $n$, $\\Gamma(m+n+1) = (m+n)!$, so the series simplifies.

The first few values:
- $J_0(0) = 1$, $J_0(2.4048) \\approx 0$ (first zero)
- $J_1(0) = 0$, $J_1(1) \\approx 0.4401$

## Recurrence Relation

Given $J_0$ and $J_1$, higher orders are computed by the **forward recurrence**:

$$J_{n+1}(x) = \\frac{2n}{x} J_n(x) - J_{n-1}(x)$$

This recurrence is numerically stable for computing $J_n$ when going upward in $n$ at fixed $x > 0$.

## Important Zeros

| Function | First zero | Second zero |
|----------|-----------|-------------|
| $J_0(x)$ | 2.4048    | 5.5201      |
| $J_1(x)$ | 3.8317    | 7.0156      |

These zeros determine the resonant frequencies in cylindrical cavities.

## Wronskian

The Wronskian of $J_0$ and $J_1$ satisfies:

$$J_0(x) J_1'(x) - J_1(x) J_0'(x) = -\\frac{1}{x}$$

This identity is a consequence of Bessel's equation and can be used to check numerical accuracy.

## Your Task

Implement the three Bessel function routines. Use 20-term series for $J_0$ and $J_1$. For $\\Gamma(m+n+1)$ with integer $n$, you may use the factorial $(m+n)!$. Use only Python's \`math\` module.
`,
  starterCode: `import math

def bessel_J0(x):
    pass

def bessel_J1(x):
    pass

def bessel_Jn(n, x):
    pass
`,
  solution: `import math

def bessel_J0(x):
    def factorial(k):
        result = 1
        for i in range(2, k + 1):
            result *= i
        return result
    result = 0.0
    for m in range(20):
        term = ((-1) ** m) / (factorial(m) ** 2) * (x / 2.0) ** (2 * m)
        result += term
    return result

def bessel_J1(x):
    def factorial(k):
        result = 1
        for i in range(2, k + 1):
            result *= i
        return result
    result = 0.0
    for m in range(20):
        term = ((-1) ** m) / (factorial(m) * factorial(m + 1)) * (x / 2.0) ** (2 * m + 1)
        result += term
    return result

def bessel_Jn(n, x):
    if n == 0:
        return bessel_J0(x)
    if n == 1:
        return bessel_J1(x)
    j_prev = bessel_J0(x)
    j_curr = bessel_J1(x)
    for k in range(1, n):
        j_next = (2.0 * k / x) * j_curr - j_prev
        j_prev = j_curr
        j_curr = j_next
    return j_curr
`,
  tests: [
    {
      name: "J0(0) = 1",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{bessel_J0(0):.4f}")`,
    },
    {
      name: "J0(2.4048) ≈ 0 (first zero)",
      expected: "0.0000\n",
      code: `{{FUNC}}
print(f"{bessel_J0(2.4048):.4f}")`,
    },
    {
      name: "J1(1.0)",
      expected: "0.4401\n",
      code: `{{FUNC}}
print(f"{bessel_J1(1.0):.4f}")`,
    },
    {
      name: "J2(1.0) via recurrence",
      expected: "0.1149\n",
      code: `{{FUNC}}
print(f"{bessel_Jn(2, 1.0):.4f}")`,
    },
  ],
};
