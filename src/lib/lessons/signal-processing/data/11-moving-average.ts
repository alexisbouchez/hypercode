import type { Lesson } from "../../types";

export const movingAverage: Lesson = {
	id: "moving-average",
	title: "Moving Average Filter",
	chapterId: "filters",
	content: `## Moving Average Filter

The **moving average** is the simplest digital filter. It smooths a signal by replacing each sample with the average of itself and the $M-1$ preceding samples:

$$y[n] = \\frac{1}{M} \\sum_{k=0}^{M-1} x[n-k]$$

This is a **causal** filter — it only uses past and current samples.

### Zero-Padding at the Start

For $n < M-1$, not enough past samples exist. We treat missing samples as zero:

$$y[n] = \\frac{1}{M}(x[0] + x[1] + \\cdots + x[n] + \\underbrace{0 + \\cdots + 0}_{M-1-n \\text{ zeros}})$$

This means the output array has the same length as the input.

### Example: $M = 3$

Signal: $[1, 2, 3, 4, 5]$

$$y[0] = (0 + 0 + 1)/3 = 0.3333$$
$$y[1] = (0 + 1 + 2)/3 = 1.0$$
$$y[2] = (1 + 2 + 3)/3 = 2.0$$
$$y[3] = (2 + 3 + 4)/3 = 3.0$$
$$y[4] = (3 + 4 + 5)/3 = 4.0$$

### Frequency Response

The moving average is a **low-pass filter** — it attenuates high-frequency components. A longer window ($M$ larger) gives stronger smoothing but slower response.

### Your Task

Implement \`moving_average(x, M)\` with causal zero-padding. Return a list of the same length as $x$.

\`\`\`python
def moving_average(x, M):
    result = []
    for n in range(len(x)):
        window = x[max(0, n - M + 1):n + 1]
        padded_sum = sum(window)  # missing samples are 0
        result.append(padded_sum / M)
    return result

print([round(v, 4) for v in moving_average([1, 2, 3, 4, 5], 3)])
# [0.3333, 1.0, 2.0, 3.0, 4.0]
\`\`\``,

	starterCode: `def moving_average(x, M):
    # Causal moving average: y[n] = mean of x[n-M+1..n], zero-pad missing samples
    pass

print([round(v, 4) for v in moving_average([1, 2, 3, 4, 5], 3)])
`,

	solution: `def moving_average(x, M):
    result = []
    for n in range(len(x)):
        window = x[max(0, n - M + 1):n + 1]
        padded_sum = sum(window)
        result.append(padded_sum / M)
    return result

print([round(v, 4) for v in moving_average([1, 2, 3, 4, 5], 3)])
`,

	tests: [
		{
			name: "moving_average([1,2,3,4,5], 3) = [0.3333, 1.0, 2.0, 3.0, 4.0]",
			code: `{{FUNC}}
print([round(v, 4) for v in moving_average([1, 2, 3, 4, 5], 3)])`,
			expected: "[0.3333, 1.0, 2.0, 3.0, 4.0]\n",
		},
		{
			name: "moving_average([1,1,1,1], 2) = [0.5, 1.0, 1.0, 1.0]",
			code: `{{FUNC}}
print([round(v, 4) for v in moving_average([1.0, 1.0, 1.0, 1.0], 2)])`,
			expected: "[0.5, 1.0, 1.0, 1.0]\n",
		},
		{
			name: "output length equals input length",
			code: `{{FUNC}}
result = moving_average([1, 2, 3, 4, 5, 6, 7, 8], 4)
print(len(result))`,
			expected: "8\n",
		},
		{
			name: "moving_average([0,0,0,3,0], 3) = [0.0, 0.0, 0.0, 1.0, 1.0]",
			code: `{{FUNC}}
print([round(v, 4) for v in moving_average([0, 0, 0, 3, 0], 3)])`,
			expected: "[0.0, 0.0, 0.0, 1.0, 1.0]\n",
		},
		{
			name: "moving_average M=1 is identity",
			code: `{{FUNC}}
x = [1.0, 3.0, 5.0, 7.0]
print([round(v, 4) for v in moving_average(x, 1)])`,
			expected: "[1.0, 3.0, 5.0, 7.0]\n",
		},
	],
};
