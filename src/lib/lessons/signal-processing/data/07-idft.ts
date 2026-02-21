import type { Lesson } from "../../types";

export const idft: Lesson = {
	id: "idft",
	title: "Inverse DFT",
	chapterId: "fourier",
	content: `## Inverse Discrete Fourier Transform

The **Inverse DFT (IDFT)** reconstructs the original time-domain signal from its frequency-domain representation. It is defined as:

$$x[n] = \\frac{1}{N} \\sum_{k=0}^{N-1} X[k] \\cdot e^{2\\pi j k n / N}, \\quad n = 0, 1, \\ldots, N-1$$

Notice:
- The exponent is **positive** (versus negative for DFT)
- There is a **$1/N$ normalization** factor

### DFT–IDFT Round Trip

The DFT and IDFT are exact inverses (up to floating-point precision):

$$\\text{IDFT}(\\text{DFT}(x)) = x$$

This round-trip property is essential for signal reconstruction after frequency-domain processing.

### Example

Given $X = [4, 0, 0, 0]$:

$$x[n] = \\frac{1}{4} \\sum_{k=0}^{3} X[k] \\cdot e^{2\\pi j k n/4} = \\frac{1}{4} \\cdot 4 = 1 \\quad \\text{for all } n$$

Result: $[1, 1, 1, 1]$ — a DC signal.

### Your Task

Implement:
- \`idft(X)\` — returns a list of $N$ real floats (take \`.real\` of each result)

\`\`\`python
import math

def idft(X):
    N = len(X)
    x = []
    for n in range(N):
        val = 0 + 0j
        for k in range(N):
            val += X[k] * math.e ** (2j * math.pi * k * n / N)
        x.append((val / N).real)
    return x
\`\`\`

Verify the round trip: define \`dft\` from the previous lesson and check \`idft(dft([1,2,3,4])) ≈ [1,2,3,4]\`.`,

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

def idft(X):
    # Return list of N real floats: (1/N) * sum(X[k] * e^(2j*pi*k*n/N))
    pass

orig = [1.0, 2.0, 3.0, 4.0]
recovered = idft(dft(orig))
print([round(v, 4) for v in recovered])
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

def idft(X):
    N = len(X)
    x = []
    for n in range(N):
        val = 0 + 0j
        for k in range(N):
            val += X[k] * math.e ** (2j * math.pi * k * n / N)
        x.append((val / N).real)
    return x

orig = [1.0, 2.0, 3.0, 4.0]
recovered = idft(dft(orig))
print([round(v, 4) for v in recovered])
`,

	tests: [
		{
			name: "idft(dft([1,2,3,4])) round-trips correctly",
			code: `{{FUNC}}
orig = [1.0, 2.0, 3.0, 4.0]
recovered = idft(dft(orig))
print([round(v, 4) for v in recovered])`,
			expected: "[1.0, 2.0, 3.0, 4.0]\n",
		},
		{
			name: "idft([4,0,0,0]) = [1.0, 1.0, 1.0, 1.0]",
			code: `{{FUNC}}
X = [4+0j, 0+0j, 0+0j, 0+0j]
result = idft(X)
print([round(v, 4) for v in result])`,
			expected: "[1.0, 1.0, 1.0, 1.0]\n",
		},
		{
			name: "idft output length equals input length",
			code: `{{FUNC}}
result = idft([1+0j, 0+0j, 0+0j, 0+0j, 0+0j, 0+0j, 0+0j, 0+0j])
print(len(result))`,
			expected: "8\n",
		},
		{
			name: "round-trip of [5, 3, 1, 7]",
			code: `{{FUNC}}
orig = [5.0, 3.0, 1.0, 7.0]
recovered = idft(dft(orig))
print([round(v, 4) for v in recovered])`,
			expected: "[5.0, 3.0, 1.0, 7.0]\n",
		},
	],
};
