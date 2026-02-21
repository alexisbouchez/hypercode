import type { Lesson } from "../../types";

export const convolution: Lesson = {
	id: "convolution",
	title: "Convolution",
	chapterId: "fundamentals",
	content: `## Convolution

**Convolution** is the central operation of linear time-invariant (LTI) system theory. The output of any LTI system is the convolution of the input signal with the system's **impulse response**.

### Linear Convolution

Given signals $x$ of length $N$ and $h$ of length $M$, their convolution $y = x * h$ has length $N + M - 1$:

$$y[k] = \\sum_{n=0}^{N-1} x[n] \\cdot h[k - n]$$

where $h[k-n] = 0$ when $k - n$ is out of bounds.

### How to Compute It

For each output index $k$ from $0$ to $N+M-2$:
- Slide $h$ over $x$, multiply corresponding elements, sum the products.

### Example

$x = [1, 2, 3]$ convolved with $h = [1, 1]$:

$$y[0] = 1 \\cdot 1 = 1$$
$$y[1] = 2 \\cdot 1 + 1 \\cdot 1 = 3$$
$$y[2] = 3 \\cdot 1 + 2 \\cdot 1 = 5$$
$$y[3] = 3 \\cdot 1 = 3$$

Result: $[1, 3, 5, 3]$

### Applications

- **Filtering** — convolving with a low-pass kernel smooths the signal
- **Echo** — adding a delayed copy of the signal
- **Cross-correlation** — closely related to convolution

### Your Task

Implement \`convolve(x, h)\` that returns the full linear convolution of lists $x$ and $h$ (no imports needed).

\`\`\`python
def convolve(x, h):
    N = len(x) + len(h) - 1
    result = [0.0] * N
    for i, xi in enumerate(x):
        for j, hj in enumerate(h):
            result[i + j] += xi * hj
    return result

print(convolve([1, 2, 3], [1, 1]))  # [1.0, 3.0, 5.0, 3.0]
\`\`\``,

	starterCode: `def convolve(x, h):
    # Compute full linear convolution; output length = len(x) + len(h) - 1
    pass

print(convolve([1, 2, 3], [1, 1]))
print(convolve([1, 2], [3, 4]))
`,

	solution: `def convolve(x, h):
    N = len(x) + len(h) - 1
    result = [0.0] * N
    for i, xi in enumerate(x):
        for j, hj in enumerate(h):
            result[i + j] += xi * hj
    return result

print(convolve([1, 2, 3], [1, 1]))
print(convolve([1, 2], [3, 4]))
`,

	tests: [
		{
			name: "convolve([1,2,3], [1,1]) = [1.0, 3.0, 5.0, 3.0]",
			code: `{{FUNC}}
print(convolve([1, 2, 3], [1, 1]))`,
			expected: "[1.0, 3.0, 5.0, 3.0]\n",
		},
		{
			name: "convolve([1,2], [3,4]) = [3.0, 10.0, 8.0]",
			code: `{{FUNC}}
print(convolve([1, 2], [3, 4]))`,
			expected: "[3.0, 10.0, 8.0]\n",
		},
		{
			name: "convolve([1], [1,2,3]) = [1.0, 2.0, 3.0] (identity convolution)",
			code: `{{FUNC}}
print(convolve([1], [1, 2, 3]))`,
			expected: "[1.0, 2.0, 3.0]\n",
		},
		{
			name: "output length = len(x) + len(h) - 1",
			code: `{{FUNC}}
result = convolve([1, 0, 0], [1, 2, 3])
print(len(result))`,
			expected: "5\n",
		},
		{
			name: "convolve([1,0,0], [1,2,3]) = [1.0, 2.0, 3.0, 0.0, 0.0]",
			code: `{{FUNC}}
print(convolve([1, 0, 0], [1, 2, 3]))`,
			expected: "[1.0, 2.0, 3.0, 0.0, 0.0]\n",
		},
	],
};
