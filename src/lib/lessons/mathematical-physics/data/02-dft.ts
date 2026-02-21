import type { Lesson } from "../../types";

export const dft: Lesson = {
  id: "dft",
  title: "Discrete Fourier Transform",
  chapterId: "transforms",
  content: `# Discrete Fourier Transform

The **Discrete Fourier Transform (DFT)** converts a finite sequence of N samples $x[0], x[1], \\ldots, x[N-1]$ into a sequence of complex frequency components $X[0], X[1], \\ldots, X[N-1]$:

$$X[k] = \\sum_{n=0}^{N-1} x[n]\\, e^{-2\\pi i k n / N}$$

The DFT reveals which frequencies are present in the signal.

## Power Spectrum

The **power spectrum** measures the energy at each frequency:

$$P[k] = \\frac{|X[k]|^2}{N^2}$$

For a pure cosine $x[n] = \\cos(2\\pi k_0 n / N)$, the power spectrum has peaks at $k = k_0$ and $k = N - k_0$ (the aliased component), each with value $0.25$.

## Inverse DFT

The **inverse DFT** reconstructs the original signal from frequency components:

$$x[n] = \\frac{1}{N} \\sum_{k=0}^{N-1} X[k]\\, e^{2\\pi i k n / N}$$

The round-trip $\\text{IDFT}(\\text{DFT}(x)) = x$ is exact (up to floating-point precision).

## Frequency Resolution

If the signal has a sampling interval $\\Delta t$, the frequency resolution is:

$$\\Delta f = \\frac{1}{N \\Delta t}$$

Frequencies range from $0$ to $(N-1)/(N \\Delta t)$, and the Nyquist frequency is $1/(2\\Delta t)$.

## Complexity

The direct DFT requires $O(N^2)$ operations. The Fast Fourier Transform (FFT) reduces this to $O(N \\log N)$ by exploiting symmetry, but here we implement the straightforward $O(N^2)$ version to understand the definition.

## Your Task

Implement the DFT, power spectrum, and inverse DFT. Represent complex numbers as (real, imag) tuples. Use only Python's built-in \`math\` and \`cmath\` modules.
`,
  starterCode: `import math
import cmath

def dft(x_list):
    pass

def power_spectrum(x_list):
    pass

def idft(X_list):
    pass
`,
  solution: `import math
import cmath

def dft(x_list):
    N = len(x_list)
    X = []
    for k in range(N):
        val = sum(x_list[n] * cmath.exp(-2j * math.pi * k * n / N) for n in range(N))
        X.append((val.real, val.imag))
    return X

def power_spectrum(x_list):
    N = len(x_list)
    X = dft(x_list)
    return [(re ** 2 + im ** 2) / (N ** 2) for re, im in X]

def idft(X_list):
    N = len(X_list)
    result = []
    for n in range(N):
        val = sum((X_list[k][0] + 1j * X_list[k][1]) * cmath.exp(2j * math.pi * k * n / N) for k in range(N))
        result.append(val.real / N)
    return result
`,
  tests: [
    {
      name: "power spectrum peak at k=1 for cosine",
      expected: "0.2500\n",
      code: `{{FUNC}}
import math
x = [math.cos(2*math.pi*k/8) for k in range(8)]
print(f"{power_spectrum(x)[1]:.4f}")`,
    },
    {
      name: "aliased component at k=7",
      expected: "0.2500\n",
      code: `{{FUNC}}
import math
x = [math.cos(2*math.pi*k/8) for k in range(8)]
print(f"{power_spectrum(x)[7]:.4f}")`,
    },
    {
      name: "DC component near zero",
      expected: "0.0000\n",
      code: `{{FUNC}}
import math
x = [math.cos(2*math.pi*k/8) for k in range(8)]
print(f"{power_spectrum(x)[0]:.4f}")`,
    },
    {
      name: "round-trip IDFT(DFT(x)) first element",
      expected: "1.0000\n",
      code: `{{FUNC}}
x = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
X = dft(x)
rec = idft(X)
print(f"{rec[0]:.4f}")`,
    },
  ],
};
