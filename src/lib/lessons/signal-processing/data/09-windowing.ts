import type { Lesson } from "../../types";

export const windowing: Lesson = {
	id: "windowing",
	title: "Windowing",
	chapterId: "fourier",
	content: `## Windowing

When computing the DFT of a finite signal, sharp edges at the start and end of the frame cause **spectral leakage** — energy from one frequency bin "leaks" into neighboring bins. A **window function** tapers the signal to zero at the edges, suppressing this effect.

### The Hann Window

The **Hann window** (also called Hanning) is the most commonly used:

$$w[n] = 0.5 \\left(1 - \\cos\\!\\left(\\frac{2\\pi n}{N-1}\\right)\\right), \\quad n = 0, 1, \\ldots, N-1$$

Properties:
- $w[0] = w[N-1] = 0$ (tapers to zero at edges)
- $w[(N-1)/2] = 1.0$ (peak at center)
- Smooth roll-off reduces leakage dramatically

### Example: 4-Point Hann Window

$$w[0] = 0.5(1 - \\cos(0)) = 0.0$$
$$w[1] = 0.5\\left(1 - \\cos\\!\\left(\\frac{2\\pi}{3}\\right)\\right) = 0.75$$
$$w[2] = 0.5\\left(1 - \\cos\\!\\left(\\frac{4\\pi}{3}\\right)\\right) = 0.75$$
$$w[3] = 0.5\\left(1 - \\cos(2\\pi)\\right) = 0.0$$

### Applying the Window

Windowing is an elementwise multiplication before the DFT:

$$x_w[n] = x[n] \\cdot w[n]$$

### Your Task

Implement:
- \`hann_window(N)\` — returns $[w[0], \\ldots, w[N-1]]$
- \`apply_window(x, w)\` — returns elementwise product $[x[n] \\cdot w[n]]$

\`\`\`python
import math

def hann_window(N):
    return [0.5 * (1 - math.cos(2 * math.pi * n / (N - 1))) for n in range(N)]

def apply_window(x, w):
    return [xi * wi for xi, wi in zip(x, w)]

print([round(v, 4) for v in hann_window(4)])  # [0.0, 0.75, 0.75, 0.0]
\`\`\``,

	starterCode: `import math

def hann_window(N):
    # Return [0.5*(1-cos(2*pi*n/(N-1))) for n in range(N)]
    pass

def apply_window(x, w):
    # Return elementwise product of x and w
    pass

print([round(v, 4) for v in hann_window(4)])
`,

	solution: `import math

def hann_window(N):
    return [0.5 * (1 - math.cos(2 * math.pi * n / (N - 1))) for n in range(N)]

def apply_window(x, w):
    return [xi * wi for xi, wi in zip(x, w)]

print([round(v, 4) for v in hann_window(4)])
`,

	tests: [
		{
			name: "hann_window(4) = [0.0, 0.75, 0.75, 0.0]",
			code: `{{FUNC}}
print([round(v, 4) for v in hann_window(4)])`,
			expected: "[0.0, 0.75, 0.75, 0.0]\n",
		},
		{
			name: "hann_window(8) has correct values",
			code: `{{FUNC}}
print([round(v, 4) for v in hann_window(8)])`,
			expected: "[0.0, 0.1883, 0.6113, 0.9505, 0.9505, 0.6113, 0.1883, 0.0]\n",
		},
		{
			name: "apply_window([1,2,3,4], hann_window(4))",
			code: `{{FUNC}}
result = apply_window([1.0, 2.0, 3.0, 4.0], hann_window(4))
print([round(v, 4) for v in result])`,
			expected: "[0.0, 1.5, 2.25, 0.0]\n",
		},
		{
			name: "hann_window output length equals N",
			code: `{{FUNC}}
print(len(hann_window(16)))`,
			expected: "16\n",
		},
		{
			name: "hann_window edges are zero",
			code: `{{FUNC}}
w = hann_window(10)
print(round(w[0], 4), round(w[-1], 4))`,
			expected: "0.0 0.0\n",
		},
	],
};
