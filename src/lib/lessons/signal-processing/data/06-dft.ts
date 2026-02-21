import type { Lesson } from "../../types";

export const dft: Lesson = {
	id: "dft",
	title: "Discrete Fourier Transform",
	chapterId: "fourier",
	content: `## Discrete Fourier Transform

The **Discrete Fourier Transform (DFT)** transforms a finite sequence of $N$ samples from the time domain into the frequency domain. It is the cornerstone of all digital signal processing.

### Definition

$$X[k] = \\sum_{n=0}^{N-1} x[n] \\cdot e^{-2\\pi j k n / N}, \\quad k = 0, 1, \\ldots, N-1$$

Each output $X[k]$ is a complex number representing the amplitude and phase of the frequency component at:

$$f_k = k \\cdot \\frac{f_s}{N}$$

where $f_s$ is the sampling rate.

### Magnitude Spectrum

The **magnitude spectrum** $|X[k]|$ shows how much energy is at each frequency. The **phase spectrum** $\\angle X[k]$ shows the phase offset.

### Example

DFT of $[1, 0, 0, 0]$ (a single impulse):

$$X[k] = \\sum_{n=0}^{3} \\delta[n] \\cdot e^{-2\\pi j k n/4} = e^0 = 1 \\quad \\text{for all } k$$

All frequency bins have equal magnitude $= 1.0$ — an impulse contains all frequencies equally.

DFT of $[1, 1, 1, 1]$:

$$X[0] = 4, \\quad X[1] = X[2] = X[3] = 0$$

A DC signal (constant) has energy only at frequency $0$.

### Your Task

Implement:
- \`dft(x)\` — returns a list of $N$ complex numbers $X[k]$
- \`dft_magnitude(x)\` — returns $[|X[0]|, |X[1]|, \\ldots, |X[N-1]|]$

\`\`\`python
import math

def dft(x):
    N = len(x)
    X = []
    for k in range(N):
        val = 0 + 0j
        for n in range(N):
            val += x[n] * math.e ** (-2j * math.pi * k * n / N)
        X.append(val)
    return X

def dft_magnitude(x):
    return [abs(v) for v in dft(x)]
\`\`\``,

	starterCode: `import math

def dft(x):
    # Return list of N complex numbers X[k] = sum(x[n] * e^(-2j*pi*k*n/N))
    pass

def dft_magnitude(x):
    # Return list of |X[k]| for each k
    pass

mags = dft_magnitude([1, 0, 0, 0])
print([round(m, 4) for m in mags])
`,

	solution: `import math

def dft(x):
    N = len(x)
    X = []
    for k in range(N):
        val = 0 + 0j
        for n in range(N):
            val += x[n] * math.e ** (-2j * math.pi * k * n / N)
        X.append(val)
    return X

def dft_magnitude(x):
    return [abs(v) for v in dft(x)]

mags = dft_magnitude([1, 0, 0, 0])
print([round(m, 4) for m in mags])
`,

	tests: [
		{
			name: "dft_magnitude([1,0,0,0]) = [1.0, 1.0, 1.0, 1.0]",
			code: `{{FUNC}}
mags = dft_magnitude([1, 0, 0, 0])
print([round(m, 4) for m in mags])`,
			expected: "[1.0, 1.0, 1.0, 1.0]\n",
		},
		{
			name: "dft([1,1,1,1]) has X[0]=4 and X[1]=X[2]=X[3]=0",
			code: `{{FUNC}}
X = dft([1, 1, 1, 1])
print([round(v.real, 4) for v in X])`,
			expected: "[4.0, -0.0, 0.0, 0.0]\n",
		},
		{
			name: "dft_magnitude([1,0,1,0]) = [2.0, 0.0, 2.0, 0.0]",
			code: `{{FUNC}}
mags = dft_magnitude([1, 0, 1, 0])
print([round(m, 4) for m in mags])`,
			expected: "[2.0, 0.0, 2.0, 0.0]\n",
		},
		{
			name: "dft([1,0,0,0]) has all real parts = 1.0",
			code: `{{FUNC}}
X = dft([1, 0, 0, 0])
print([round(v.real, 4) for v in X])`,
			expected: "[1.0, 1.0, 1.0, 1.0]\n",
		},
		{
			name: "dft output length equals input length",
			code: `{{FUNC}}
X = dft([1, 2, 3, 4, 5, 6, 7, 8])
print(len(X))`,
			expected: "8\n",
		},
	],
};
