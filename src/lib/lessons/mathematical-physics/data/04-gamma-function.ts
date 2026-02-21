import type { Lesson } from "../../types";

export const gammaFunction: Lesson = {
  id: "gamma-function",
  title: "Gamma Function",
  chapterId: "special-functions",
  content: `# Gamma Function

The **Gamma function** $\\Gamma(n)$ generalizes the factorial to real (and complex) numbers:

$$\\Gamma(n) = \\int_0^\\infty t^{n-1} e^{-t}\\, dt$$

For positive integers: $\\Gamma(n) = (n-1)!$

## Key Properties

- **Recurrence:** $\\Gamma(n+1) = n\\, \\Gamma(n)$
- **Half-integer:** $\\Gamma(1/2) = \\sqrt{\\pi}$, $\\Gamma(3/2) = \\frac{1}{2}\\sqrt{\\pi}$
- **Reflection formula:** $\\Gamma(x)\\, \\Gamma(1-x) = \\frac{\\pi}{\\sin(\\pi x)}$

## Stirling's Approximation

For large $n$, the log-gamma function is approximated by:

$$\\ln \\Gamma(n) \\approx \\left(n - \\tfrac{1}{2}\\right) \\ln n - n + \\tfrac{1}{2} \\ln(2\\pi)$$

This is the basis for \`log_gamma(n)\`.

## Lanczos Approximation

The **Lanczos approximation** computes $\\Gamma(z)$ accurately for positive reals using a set of precomputed coefficients. For $n \\geq 0.5$, let $z = n - 1$:

$$\\Gamma(z+1) = \\sqrt{2\\pi}\\, t^{z+0.5}\\, e^{-t}\\, A_g(z)$$

where $t = z + g + 0.5$ and:

$$A_g(z) = c_0 + \\sum_{k=1}^{g+1} \\frac{c_k}{z + k}$$

For $n < 0.5$, use the reflection formula:

$$\\Gamma(n) = \\frac{\\pi}{\\sin(\\pi n)\\, \\Gamma(1-n)}$$

## Beta Function

The **Beta function** is defined as:

$$B(a, b) = \\frac{\\Gamma(a)\\, \\Gamma(b)}{\\Gamma(a+b)} = \\int_0^1 t^{a-1}(1-t)^{b-1}\\, dt$$

It arises in probability (Beta distribution), combinatorics, and integration.

## Your Task

Implement \`gamma\` using the Lanczos approximation with $g=7$ and the 9-coefficient vector below. Implement \`log_gamma\` using Stirling's approximation. Implement \`beta_function\` using the Gamma function.

Lanczos coefficients (g=7):
\`\`\`
[0.99999999999980993, 676.5203681218851, -1259.1392167224028,
 771.32342877765313, -176.61502916214059, 12.507343278686905,
 -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]
\`\`\`
`,
  starterCode: `import math

def gamma(n):
    pass

def log_gamma(n):
    pass

def beta_function(a, b):
    pass
`,
  solution: `import math

def gamma(n):
    if n < 0.5:
        return math.pi / (math.sin(math.pi * n) * gamma(1.0 - n))
    z = n - 1.0
    g = 7
    p = [
        0.99999999999980993,
        676.5203681218851,
        -1259.1392167224028,
        771.32342877765313,
        -176.61502916214059,
        12.507343278686905,
        -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7,
    ]
    x = p[0]
    for i in range(1, g + 2):
        x += p[i] / (z + i)
    t = z + g + 0.5
    return math.sqrt(2.0 * math.pi) * t ** (z + 0.5) * math.exp(-t) * x

def log_gamma(n):
    return (n - 0.5) * math.log(n) - n + 0.5 * math.log(2.0 * math.pi)

def beta_function(a, b):
    return gamma(a) * gamma(b) / gamma(a + b)
`,
  tests: [
    {
      name: "gamma(5) = 24",
      expected: "24.0000\n",
      code: `{{FUNC}}
print(f"{gamma(5):.4f}")`,
    },
    {
      name: "gamma(0.5) = sqrt(pi)",
      expected: "1.7725\n",
      code: `{{FUNC}}
print(f"{gamma(0.5):.4f}")`,
    },
    {
      name: "gamma(1.5) = 0.5*sqrt(pi)",
      expected: "0.8862\n",
      code: `{{FUNC}}
print(f"{gamma(1.5):.4f}")`,
    },
    {
      name: "beta(2, 3) = 1/12",
      expected: "0.0833\n",
      code: `{{FUNC}}
print(f"{beta_function(2, 3):.4f}")`,
    },
  ],
};
