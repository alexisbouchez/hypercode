import type { Lesson } from "../../types";

export const fourierSeries: Lesson = {
  id: "fourier-series",
  title: "Fourier Series",
  chapterId: "transforms",
  content: `# Fourier Series

A **Fourier series** decomposes a periodic function into a sum of sines and cosines. For a function f(x) with period 2L, the series is:

$$f_N(x) = \\frac{a_0}{2} + \\sum_{n=1}^{N} \\left[ a_n \\cos\\left(\\frac{n\\pi x}{L}\\right) + b_n \\sin\\left(\\frac{n\\pi x}{L}\\right) \\right]$$

The Fourier coefficients are computed via integration:

- **DC component:** $a_0 = \\frac{1}{L} \\int_{-L}^{L} f(x)\\, dx$
- **Cosine coefficients:** $a_n = \\frac{1}{L} \\int_{-L}^{L} f(x) \\cos\\left(\\frac{n\\pi x}{L}\\right) dx$
- **Sine coefficients:** $b_n = \\frac{1}{L} \\int_{-L}^{L} f(x) \\sin\\left(\\frac{n\\pi x}{L}\\right) dx$

## Numerical Integration

We represent f(x) by N evenly spaced sample points on [-L, L) using a **midpoint grid**:

$$x_i = -L + \\frac{2L(i + 0.5)}{N}, \\quad i = 0, 1, \\ldots, N-1$$

The step size is $\\Delta x = 2L/N$. The integrals become sums:

$$a_0 \\approx \\frac{1}{L} \\sum_{i=0}^{N-1} f(x_i)\\, \\Delta x$$

$$a_n \\approx \\frac{1}{L} \\sum_{i=0}^{N-1} f(x_i) \\cos\\left(\\frac{n\\pi x_i}{L}\\right) \\Delta x$$

$$b_n \\approx \\frac{1}{L} \\sum_{i=0}^{N-1} f(x_i) \\sin\\left(\\frac{n\\pi x_i}{L}\\right) \\Delta x$$

## Square Wave Example

A **square wave** on $[-\\pi, \\pi]$ is $f(x) = 1$ if $x > 0$, else $-1$. Its Fourier series contains only odd sine harmonics:

$$f(x) = \\frac{4}{\\pi} \\left[ \\sin(x) + \\frac{\\sin(3x)}{3} + \\frac{\\sin(5x)}{5} + \\cdots \\right]$$

So $b_1 = 4/\\pi \\approx 1.2732$, $b_3 = 4/(3\\pi) \\approx 0.4244$, and all $a_n = 0$.

## Your Task

Implement the four Fourier series functions. Use N=1000 sample points on the midpoint grid. The \`f_values\` input is a list of N function values at the grid points.
`,
  starterCode: `import math

def fourier_a0(f_values, L):
    pass

def fourier_an(f_values, L, n):
    pass

def fourier_bn(f_values, L, n):
    pass

def fourier_reconstruct(x, a0, an_list, bn_list, L):
    pass
`,
  solution: `import math

def fourier_a0(f_values, L):
    N = len(f_values)
    dx = 2.0 * L / N
    return sum(f * dx for f in f_values) / L

def fourier_an(f_values, L, n):
    N = len(f_values)
    dx = 2.0 * L / N
    xs = [-L + 2.0 * L * (i + 0.5) / N for i in range(N)]
    return sum(f_values[i] * math.cos(n * math.pi * xs[i] / L) * dx for i in range(N)) / L

def fourier_bn(f_values, L, n):
    N = len(f_values)
    dx = 2.0 * L / N
    xs = [-L + 2.0 * L * (i + 0.5) / N for i in range(N)]
    return sum(f_values[i] * math.sin(n * math.pi * xs[i] / L) * dx for i in range(N)) / L

def fourier_reconstruct(x, a0, an_list, bn_list, L):
    result = a0 / 2.0
    for idx, an in enumerate(an_list):
        result += an * math.cos((idx + 1) * math.pi * x / L)
    for idx, bn in enumerate(bn_list):
        result += bn * math.sin((idx + 1) * math.pi * x / L)
    return result
`,
  tests: [
    {
      name: "a0 of square wave ≈ 0",
      expected: "0.0000\n",
      code: `{{FUNC}}
import math
L = math.pi
N = 1000
xs = [-L + 2.0*L*(i+0.5)/N for i in range(N)]
f_values = [1.0 if x > 0 else -1.0 for x in xs]
print(f"{fourier_a0(f_values, L):.4f}")`,
    },
    {
      name: "b1 of square wave ≈ 4/pi",
      expected: "1.2732\n",
      code: `{{FUNC}}
import math
L = math.pi
N = 1000
xs = [-L + 2.0*L*(i+0.5)/N for i in range(N)]
f_values = [1.0 if x > 0 else -1.0 for x in xs]
print(f"{fourier_bn(f_values, L, 1):.4f}")`,
    },
    {
      name: "b3 of square wave ≈ 4/(3pi)",
      expected: "0.4244\n",
      code: `{{FUNC}}
import math
L = math.pi
N = 1000
xs = [-L + 2.0*L*(i+0.5)/N for i in range(N)]
f_values = [1.0 if x > 0 else -1.0 for x in xs]
print(f"{fourier_bn(f_values, L, 3):.4f}")`,
    },
    {
      name: "partial reconstruction at x=0.5",
      expected: "1.0337\n",
      code: `{{FUNC}}
import math
print(f"{fourier_reconstruct(0.5, 0.0, [], [1.2732, 0.0, 0.4244], math.pi):.4f}")`,
    },
  ],
};
