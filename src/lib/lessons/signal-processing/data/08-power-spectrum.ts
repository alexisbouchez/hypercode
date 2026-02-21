import type { Lesson } from "../../types";

export const powerSpectrum: Lesson = {
	id: "power-spectrum",
	title: "Power Spectrum",
	chapterId: "fourier",
	content: `## Power Spectrum

The **power spectrum** shows how the power of a signal is distributed across frequencies. It is computed from the DFT magnitudes:

$$P[k] = \\frac{|X[k]|^2}{N}$$

where $N$ is the number of samples. This normalization makes the power independent of signal length.

### Frequency Resolution

For a signal sampled at $f_s$ Hz with $N$ samples, each bin $k$ corresponds to:

$$f_k = k \\cdot \\frac{f_s}{N}$$

Bin 0 ($k=0$) is the **DC component** (zero frequency). The highest meaningful frequency is at $k = N/2$ (the Nyquist frequency).

### Dominant Frequency

The **dominant frequency** is the bin with the highest power, excluding DC (bin 0). For a signal with a strong sinusoidal component, this reveals the fundamental frequency.

### Example

A 4-sample signal $[1, 0, 0, 0]$ (impulse) has flat power spectrum:

$$P[k] = \\frac{1}{4} \\quad \\text{for all } k$$

A DC signal $[1, 1, 1, 1]$ has all power at $k=0$:

$$P[0] = \\frac{16}{4} = 4, \\quad P[k] = 0 \\text{ for } k > 0$$

### Your Task

Implement:
- \`power_spectrum(x)\` — returns $[|X[0]|^2/N, \\ldots, |X[N-1]|^2/N]$
- \`dominant_frequency(x, fs)\` — returns $k^* \\cdot f_s / N$ where $k^*$ maximizes $P[k]$ for $k > 0$

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

def power_spectrum(x):
    N = len(x)
    X = dft(x)
    return [abs(v)**2 / N for v in X]

def dominant_frequency(x, fs):
    N = len(x)
    ps = power_spectrum(x)
    start = 1 if N > 1 else 0
    best_k = start + ps[start:].index(max(ps[start:]))
    return best_k * fs / N
\`\`\``,

	starterCode: `import math

def dft(x):
    N = len(x)
    X = []
    for k in range(N):
        val = 0 + 0j
        for n in range(N):
            val += x[n] * math.e ** (-2j * math.pi * k * n / N)
        X.append(val)
    return X

def power_spectrum(x):
    # Return [|X[k]|^2 / N for each k]
    pass

def dominant_frequency(x, fs):
    # Return frequency bin with max power (skip DC bin 0)
    pass

ps = power_spectrum([1, 0, 0, 0])
print([round(v, 4) for v in ps])
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

def power_spectrum(x):
    N = len(x)
    X = dft(x)
    return [abs(v)**2 / N for v in X]

def dominant_frequency(x, fs):
    N = len(x)
    ps = power_spectrum(x)
    start = 1 if N > 1 else 0
    best_k = start + ps[start:].index(max(ps[start:]))
    return best_k * fs / N

ps = power_spectrum([1, 0, 0, 0])
print([round(v, 4) for v in ps])
`,

	tests: [
		{
			name: "power_spectrum([1,0,0,0]) = [0.25, 0.25, 0.25, 0.25]",
			code: `{{FUNC}}
ps = power_spectrum([1, 0, 0, 0])
print([round(v, 4) for v in ps])`,
			expected: "[0.25, 0.25, 0.25, 0.25]\n",
		},
		{
			name: "power_spectrum([1,1,1,1]) = [4.0, 0.0, 0.0, 0.0]",
			code: `{{FUNC}}
ps = power_spectrum([1, 1, 1, 1])
print([round(v, 4) for v in ps])`,
			expected: "[4.0, 0.0, 0.0, 0.0]\n",
		},
		{
			name: "dominant_frequency of cosine at fs/4 = 1.0",
			code: `{{FUNC}}
import math
x = [math.cos(2 * math.pi * 1 * n / 4) for n in range(4)]
print(dominant_frequency(x, 4))`,
			expected: "1.0\n",
		},
		{
			name: "power spectrum output length equals input length",
			code: `{{FUNC}}
ps = power_spectrum([1, 2, 3, 4, 5, 6, 7, 8])
print(len(ps))`,
			expected: "8\n",
		},
	],
};
