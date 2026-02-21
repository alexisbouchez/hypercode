import type { Lesson } from "../../types";

export const iirFirstOrder: Lesson = {
	id: "iir-first-order",
	title: "IIR Filters",
	chapterId: "filters",
	content: `## IIR Filters

An **Infinite Impulse Response (IIR)** filter uses feedback — the output depends on both the input and previous outputs. The simplest first-order IIR:

$$y[n] = b \\cdot x[n] + a \\cdot y[n-1]$$

with $y[-1] = 0$ (causal initialization).

### Stability

The filter is stable when $|a| < 1$. If $a \\geq 1$, the output diverges.

### Exponential Smoother

A common application is the **exponential moving average (EMA)**:

$$y[n] = \\alpha \\cdot x[n] + (1-\\alpha) \\cdot y[n-1]$$

This is \`iir_filter(x, a=1-alpha, b=alpha)\`. The parameter $\\alpha \\in (0,1)$ controls smoothing:
- $\\alpha$ close to 1: fast response, little smoothing
- $\\alpha$ close to 0: slow response, heavy smoothing

### Example

Signal $[1, 0, 0, 0]$ through filter with $a=0.5$, $b=1$:

$$y[0] = 1 \\cdot 1 + 0.5 \\cdot 0 = 1.0$$
$$y[1] = 1 \\cdot 0 + 0.5 \\cdot 1 = 0.5$$
$$y[2] = 1 \\cdot 0 + 0.5 \\cdot 0.5 = 0.25$$
$$y[3] = 1 \\cdot 0 + 0.5 \\cdot 0.25 = 0.125$$

The impulse response decays exponentially — hence "infinite" (theoretically never reaches zero).

### Your Task

Implement:
- \`iir_filter(x, a, b)\` — causal first-order IIR: $y[n] = b \\cdot x[n] + a \\cdot y[n-1]$
- \`exponential_smoother(x, alpha)\` — calls \`iir_filter(x, 1-alpha, alpha)\`

\`\`\`python
def iir_filter(x, a, b):
    y = []
    y_prev = 0.0
    for xi in x:
        yi = b * xi + a * y_prev
        y.append(yi)
        y_prev = yi
    return y

def exponential_smoother(x, alpha):
    return iir_filter(x, 1 - alpha, alpha)
\`\`\``,

	starterCode: `def iir_filter(x, a, b):
    # y[n] = b*x[n] + a*y[n-1], y[-1] = 0
    pass

def exponential_smoother(x, alpha):
    # Call iir_filter with a=1-alpha, b=alpha
    pass

print([round(v, 4) for v in iir_filter([1.0, 0.0, 0.0, 0.0], 0.5, 1.0)])
print([round(v, 4) for v in exponential_smoother([1.0, 1.0, 1.0, 1.0], 0.5)])
`,

	solution: `def iir_filter(x, a, b):
    y = []
    y_prev = 0.0
    for xi in x:
        yi = b * xi + a * y_prev
        y.append(yi)
        y_prev = yi
    return y

def exponential_smoother(x, alpha):
    return iir_filter(x, 1 - alpha, alpha)

print([round(v, 4) for v in iir_filter([1.0, 0.0, 0.0, 0.0], 0.5, 1.0)])
print([round(v, 4) for v in exponential_smoother([1.0, 1.0, 1.0, 1.0], 0.5)])
`,

	tests: [
		{
			name: "iir_filter([1,0,0,0], a=0.5, b=1) decays exponentially",
			code: `{{FUNC}}
print([round(v, 4) for v in iir_filter([1.0, 0.0, 0.0, 0.0], 0.5, 1.0)])`,
			expected: "[1.0, 0.5, 0.25, 0.125]\n",
		},
		{
			name: "exponential_smoother converges to 1",
			code: `{{FUNC}}
print([round(v, 4) for v in exponential_smoother([1.0, 1.0, 1.0, 1.0], 0.5)])`,
			expected: "[0.5, 0.75, 0.875, 0.9375]\n",
		},
		{
			name: "exponential_smoother with alpha=0.3 on step signal",
			code: `{{FUNC}}
print([round(v, 4) for v in exponential_smoother([0.0, 0.0, 1.0, 1.0], 0.3)])`,
			expected: "[0.0, 0.0, 0.3, 0.51]\n",
		},
		{
			name: "iir_filter output length equals input length",
			code: `{{FUNC}}
result = iir_filter([1, 2, 3, 4, 5], 0.5, 0.5)
print(len(result))`,
			expected: "5\n",
		},
		{
			name: "iir_filter with a=0 is memoryless (b*x[n])",
			code: `{{FUNC}}
result = iir_filter([2.0, 4.0, 6.0], 0.0, 0.5)
print([round(v, 4) for v in result])`,
			expected: "[1.0, 2.0, 3.0]\n",
		},
	],
};
