import type { Lesson } from "../../types";

export const correlation: Lesson = {
	id: "correlation",
	title: "Cross-Correlation",
	chapterId: "applications",
	content: `## Cross-Correlation

**Cross-correlation** measures the similarity between two signals as a function of **time lag**. It is used in radar, sonar, communications, and audio fingerprinting.

### Definition

The normalized cross-correlation at lag $\\ell$ for periodic signals of length $N$:

$$R_{xy}[\\ell] = \\frac{1}{N} \\sum_{n=0}^{N-1} x[n] \\cdot y[(n + \\ell) \\bmod N]$$

The modulo wraps around the signal, treating it as periodic (circular cross-correlation).

### Autocorrelation

The **autocorrelation** of a signal with itself:

$$R_{xx}[\\ell] = \\frac{1}{N} \\sum_{n=0}^{N-1} x[n] \\cdot x[(n + \\ell) \\bmod N]$$

At lag $0$, autocorrelation equals signal power. It is symmetric: $R_{xx}[\\ell] = R_{xx}[-\\ell]$.

### Applications

- **Time delay estimation**: find the lag where $R_{xy}$ is maximized to locate a signal source
- **Pitch detection**: autocorrelation peaks at lags corresponding to the period
- **Pattern matching**: cross-correlate a template with a signal to find occurrences

### Example

$x = [1, 1, -1, -1]$, $y = [1, 1, -1, -1]$ (same signal):

- Lag 0: $(1\\cdot1 + 1\\cdot1 + (-1)(-1) + (-1)(-1))/4 = 4/4 = 1.0$
- Lag 1: $(1\\cdot1 + 1\\cdot(-1) + (-1)(-1) + (-1)\\cdot1)/4 = 0/4 = 0.0$
- Lag 2: $(1\\cdot(-1) + 1\\cdot(-1) + (-1)\\cdot1 + (-1)\\cdot1)/4 = -4/4 = -1.0$

### Your Task

Implement:
- \`xcorr(x, y, lag)\` — normalized circular cross-correlation at lag $\\ell$
- \`autocorrelation(x, lag)\` — calls \`xcorr(x, x, lag)\`

\`\`\`python
def xcorr(x, y, lag):
    N = len(x)
    return sum(x[n] * y[(n + lag) % N] for n in range(N)) / N

def autocorrelation(x, lag):
    return xcorr(x, x, lag)
\`\`\``,

	starterCode: `def xcorr(x, y, lag):
    # Return (1/N) * sum(x[n] * y[(n+lag) % N])
    pass

def autocorrelation(x, lag):
    # Return xcorr(x, x, lag)
    pass

x = [1.0, 2.0, 3.0, 4.0]
print(round(xcorr(x, x, 0), 4))
print(round(autocorrelation(x, 0), 4))
`,

	solution: `def xcorr(x, y, lag):
    N = len(x)
    return sum(x[n] * y[(n + lag) % N] for n in range(N)) / N

def autocorrelation(x, lag):
    return xcorr(x, x, lag)

x = [1.0, 2.0, 3.0, 4.0]
print(round(xcorr(x, x, 0), 4))
print(round(autocorrelation(x, 0), 4))
`,

	tests: [
		{
			name: "xcorr([1,2,3,4], [1,2,3,4], lag=0) = 7.5",
			code: `{{FUNC}}
x = [1.0, 2.0, 3.0, 4.0]
print(round(xcorr(x, x, 0), 4))`,
			expected: "7.5\n",
		},
		{
			name: "xcorr([1,2,3,4], [1,2,3,4], lag=1) = 6.0",
			code: `{{FUNC}}
x = [1.0, 2.0, 3.0, 4.0]
print(round(xcorr(x, x, 1), 4))`,
			expected: "6.0\n",
		},
		{
			name: "autocorrelation at lag 0 equals signal power",
			code: `{{FUNC}}
x = [1.0, 1.0, -1.0, -1.0]
print(round(autocorrelation(x, 0), 4))`,
			expected: "1.0\n",
		},
		{
			name: "autocorrelation at lag 2 for [1,1,-1,-1] = -1.0",
			code: `{{FUNC}}
x = [1.0, 1.0, -1.0, -1.0]
print(round(autocorrelation(x, 2), 4))`,
			expected: "-1.0\n",
		},
		{
			name: "xcorr of orthogonal signals = 0",
			code: `{{FUNC}}
x = [1.0, 0.0, -1.0, 0.0]
y = [0.0, 1.0, 0.0, -1.0]
print(round(xcorr(x, y, 0), 4))`,
			expected: "0.0\n",
		},
	],
};
