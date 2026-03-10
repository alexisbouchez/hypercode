import type { Lesson } from "../../types";

export const fundamentalTheorem: Lesson = {
	id: "fundamental-theorem",
	title: "Fundamental Theorem of Calculus",
	chapterId: "integration",
	content: `## Fundamental Theorem of Calculus

The FTC connects differentiation and integration -- the two central operations of calculus.

### Part 1

If \\(f\\) is continuous on \\([a, b]\\) and \\(F(x) = \\int_a^x f(t)\\,dt\\), then \\(F\\) is differentiable and \\(F'(x) = f(x)\\).

### Part 2

If \\(f\\) is continuous on \\([a, b]\\) and \\(F\\) is any antiderivative of \\(f\\), then:

$$\\int_a^b f(x)\\,dx = F(b) - F(a)$$

### Numerical Verification

We can verify the FTC numerically:
1. Compute \\(F(x) = \\int_a^x f(t)\\,dt\\) using numerical integration
2. Compute \\(F'(x)\\) using numerical differentiation
3. Check that \\(F'(x) \\approx f(x)\\)

### Simpson's Rule

For more accurate numerical integration, use **Simpson's rule**:

$$\\int_a^b f(x)\\,dx \\approx \\frac{\\Delta x}{3} \\left[f(x_0) + 4f(x_1) + 2f(x_2) + 4f(x_3) + \\cdots + f(x_n)\\right]$$

where \\(n\\) must be even.

### Your Task

Implement:
1. \`integrate(f, a, b, n)\` -- Simpson's rule with \\(n\\) subintervals (\\(n\\) must be even)
2. \`antiderivative_at(f, a, x, n)\` -- computes \\(F(x) = \\int_a^x f(t)\\,dt\\) using Simpson's rule
3. \`verify_ftc(f, a, x, h, n)\` -- checks that the numerical derivative of \\(F(x)\\) approximately equals \\(f(x)\\) (within 0.001). Uses \\(F'(x) \\approx (F(x+h) - F(x-h)) / (2h)\\).`,

	starterCode: `def integrate(f, a, b, n=1000):
    # Simpson's rule: n must be even
    pass

def antiderivative_at(f, a, x, n=1000):
    # Compute integral from a to x
    pass

def verify_ftc(f, a, x, h=1e-5, n=1000):
    # Check that d/dx [integral from a to x of f(t)dt] ≈ f(x)
    pass

import math

# Integral of x^2 from 0 to 1 = 1/3
print(round(integrate(lambda x: x**2, 0, 1), 6))

# Integral of sin from 0 to pi = 2
print(round(integrate(math.sin, 0, math.pi), 6))

# Verify FTC: d/dx integral(sin, 0, x) = sin(x) at x = 1
print(verify_ftc(math.sin, 0, 1))
`,

	solution: `def integrate(f, a, b, n=1000):
    if n % 2 != 0:
        n += 1
    dx = (b - a) / n
    s = f(a) + f(b)
    for i in range(1, n):
        x = a + i * dx
        if i % 2 == 0:
            s += 2 * f(x)
        else:
            s += 4 * f(x)
    return s * dx / 3

def antiderivative_at(f, a, x, n=1000):
    return integrate(f, a, x, n)

def verify_ftc(f, a, x, h=1e-5, n=1000):
    F_plus = antiderivative_at(f, a, x + h, n)
    F_minus = antiderivative_at(f, a, x - h, n)
    numerical_deriv = (F_plus - F_minus) / (2 * h)
    return abs(numerical_deriv - f(x)) < 0.001

import math

# Integral of x^2 from 0 to 1 = 1/3
print(round(integrate(lambda x: x**2, 0, 1), 6))

# Integral of sin from 0 to pi = 2
print(round(integrate(math.sin, 0, math.pi), 6))

# Verify FTC: d/dx integral(sin, 0, x) = sin(x) at x = 1
print(verify_ftc(math.sin, 0, 1))
`,

	tests: [
		{
			name: "integral of x^2 from 0 to 1 is 1/3",
			code: `{{FUNC}}
print(round(integrate(lambda x: x**2, 0, 1), 6))`,
			expected: "0.333333\n",
		},
		{
			name: "integral of sin from 0 to pi is 2",
			code: `{{FUNC}}
import math
print(round(integrate(math.sin, 0, math.pi), 6))`,
			expected: "2.0\n",
		},
		{
			name: "FTC verified for sin at x=1",
			code: `{{FUNC}}
import math
print(verify_ftc(math.sin, 0, 1))`,
			expected: "True\n",
		},
	],
};
