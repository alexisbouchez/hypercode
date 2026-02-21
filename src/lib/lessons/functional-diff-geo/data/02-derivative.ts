import type { Lesson } from "../../types";

export const derivative: Lesson = {
  id: "derivative",
  title: "The Derivative Operator",
  chapterId: "functional-foundations",
  content: `# The Derivative Operator

In standard calculus, the derivative is written $\\frac{d}{dx}f(x)$ — an expression that involves a specific variable name. This is ambiguous in complex situations. Is $x$ a free variable? Which $x$?

Sussman and Wisdom replace expression derivatives with **functional derivatives**. The operator $D$ takes a function and returns a new function — its derivative:

$$(Df)(x) = \\lim_{h \\to 0} \\frac{f(x+h) - f(x-h)}{2h}$$

This is unambiguous: $D$ is a higher-order function (a function that takes and returns functions).

## Examples

$$D(x \\mapsto x^2) = (x \\mapsto 2x)$$
$$D(x \\mapsto \\sin x) = (x \\mapsto \\cos x)$$
$$D(D(x \\mapsto x^3)) = (x \\mapsto 6x)$$

## Numerical Implementation

We approximate $D$ using the **central difference formula**:

$$Df(x) \\approx \\frac{f(x+h) - f(x-h)}{2h}$$

With $h = 10^{-7}$, this is accurate to about 14 significant figures for smooth functions.

## Your Task

Implement \`D(f)\` that returns the numerical derivative of \`f\` as a new function.
`,
  starterCode: `import math

def D(f, h=1e-7):
    pass

# D of x^2 is 2x
Df = D(lambda x: x**2)
print(f"{Df(3.0):.4f}")   # 6.0000

# D of x^3 is 3x^2
Dg = D(lambda x: x**3)
print(f"{Dg(2.0):.4f}")   # 12.0000

# D of sin is cos
print(f"{D(math.sin)(0.0):.4f}")  # 1.0000
`,
  solution: `import math

def D(f, h=1e-7):
    return lambda x: (f(x + h) - f(x - h)) / (2 * h)

Df = D(lambda x: x**2)
print(f"{Df(3.0):.4f}")

Dg = D(lambda x: x**3)
print(f"{Dg(2.0):.4f}")

print(f"{D(math.sin)(0.0):.4f}")
`,
  tests: [
    {
      name: "D(x^2)(3) ≈ 6",
      code: `{{FUNC}}
print(f"{D(lambda x: x**2)(3.0):.4f}")`,
      expected: "6.0000\n",
    },
    {
      name: "D(x^3)(2) ≈ 12",
      code: `{{FUNC}}
print(f"{D(lambda x: x**3)(2.0):.4f}")`,
      expected: "12.0000\n",
    },
    {
      name: "D(x^2 + 3x)(4) ≈ 11",
      code: `{{FUNC}}
print(f"{D(lambda x: x**2 + 3*x)(4.0):.4f}")`,
      expected: "11.0000\n",
    },
    {
      name: "D(sin)(0) ≈ 1",
      code: `{{FUNC}}
import math
print(f"{D(math.sin)(0.0):.4f}")`,
      expected: "1.0000\n",
    },
  ],
};
