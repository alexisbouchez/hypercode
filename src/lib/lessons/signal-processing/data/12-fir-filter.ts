import type { Lesson } from "../../types";

export const firFilter: Lesson = {
	id: "fir-filter",
	title: "FIR Filters",
	chapterId: "filters",
	content: `## FIR Filters

A **Finite Impulse Response (FIR)** filter is defined by its **impulse response** $h[n]$ of length $M+1$. The output is a causal convolution of the input with $h$:

$$y[n] = \\sum_{k=0}^{M} h[k] \\cdot x[n-k]$$

where $x[n-k] = 0$ for $n-k < 0$. The output has the same length as $x$.

### Why FIR?

FIR filters are:
- **Always stable** — bounded input → bounded output
- **Linear phase** — symmetric $h$ gives constant group delay (no phase distortion)
- **Flexible** — any frequency response can be approximated

### Sinc Lowpass Filter

The ideal lowpass filter with cutoff $f_c$ (normalized, $0 < f_c < 0.5$) has the impulse response:

$$h[n] = 2 f_c \\cdot \\text{sinc}(2 f_c (n - N/2))$$

where $\\text{sinc}(x) = \\sin(\\pi x) / (\\pi x)$ and $h[N/2] = 2f_c$ when $x=0$.

For $f_c = 0.25$ (quarter-band) with $N=4$:
$$h = [0, 0.3183, 0.5, 0.3183, 0]$$

### Your Task

Implement:
- \`fir_filter(x, h)\` — causal FIR convolution, output same length as $x$
- \`sinc_lowpass(fc, N)\` — returns $N+1$ coefficients for ideal lowpass

\`\`\`python
import math

def fir_filter(x, h):
    result = []
    for n in range(len(x)):
        val = 0.0
        for k in range(len(h)):
            if n - k >= 0:
                val += h[k] * x[n - k]
        result.append(val)
    return result

def sinc_lowpass(fc, N):
    h = []
    center = N // 2
    for n in range(N + 1):
        x = n - center
        if x == 0:
            h.append(2 * fc)
        else:
            h.append(2 * fc * math.sin(math.pi * 2 * fc * x) / (math.pi * 2 * fc * x))
    return h
\`\`\``,

	starterCode: `import math

def fir_filter(x, h):
    # Causal convolution: y[n] = sum(h[k]*x[n-k]), x[n-k]=0 for n-k<0
    # Output same length as x
    pass

def sinc_lowpass(fc, N):
    # Return N+1 coefficients: h[n] = 2*fc*sinc(2*fc*(n - N//2))
    # sinc(x) = sin(pi*x)/(pi*x), h[N//2] = 2*fc
    pass

h = [0.25, 0.5, 0.25]
x = [1.0, 0.0, 0.0, 0.0, 0.0]
print([round(v, 4) for v in fir_filter(x, h)])
print([round(v, 4) for v in sinc_lowpass(0.25, 4)])
`,

	solution: `import math

def fir_filter(x, h):
    result = []
    for n in range(len(x)):
        val = 0.0
        for k in range(len(h)):
            if n - k >= 0:
                val += h[k] * x[n - k]
        result.append(val)
    return result

def sinc_lowpass(fc, N):
    h = []
    center = N // 2
    for n in range(N + 1):
        x = n - center
        if x == 0:
            h.append(2 * fc)
        else:
            h.append(2 * fc * math.sin(math.pi * 2 * fc * x) / (math.pi * 2 * fc * x))
    return h

h = [0.25, 0.5, 0.25]
x = [1.0, 0.0, 0.0, 0.0, 0.0]
print([round(v, 4) for v in fir_filter(x, h)])
print([round(v, 4) for v in sinc_lowpass(0.25, 4)])
`,

	tests: [
		{
			name: "fir_filter impulse response matches h",
			code: `{{FUNC}}
h = [0.25, 0.5, 0.25]
x = [1.0, 0.0, 0.0, 0.0, 0.0]
print([round(v, 4) for v in fir_filter(x, h)])`,
			expected: "[0.25, 0.5, 0.25, 0.0, 0.0]\n",
		},
		{
			name: "fir_filter output length equals input length",
			code: `{{FUNC}}
result = fir_filter([1, 2, 3, 4, 5], [0.5, 0.5])
print(len(result))`,
			expected: "5\n",
		},
		{
			name: "sinc_lowpass(0.25, 4) = [0.0, 0.3183, 0.5, 0.3183, 0.0]",
			code: `{{FUNC}}
print([round(v, 4) for v in sinc_lowpass(0.25, 4)])`,
			expected: "[0.0, 0.3183, 0.5, 0.3183, 0.0]\n",
		},
		{
			name: "fir_filter with [0.5,0.5] kernel",
			code: `{{FUNC}}
h = [0.5, 0.5]
x = [1.0, 0.0, 0.0, 0.0]
print([round(v, 4) for v in fir_filter(x, h)])`,
			expected: "[0.5, 0.5, 0.0, 0.0]\n",
		},
		{
			name: "sinc_lowpass output length is N+1",
			code: `{{FUNC}}
print(len(sinc_lowpass(0.25, 8)))`,
			expected: "9\n",
		},
	],
};
