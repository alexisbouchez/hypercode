import type { Lesson } from "../../types";

export const sampling: Lesson = {
	id: "sampling",
	title: "Sampling and the Nyquist Theorem",
	chapterId: "fundamentals",
	content: `## Sampling and the Nyquist Theorem

Digital signals are created by **sampling** a continuous signal at discrete time steps. The sampling rate $f_s$ (in Hz) determines what frequencies can be faithfully captured.

### The Nyquist–Shannon Sampling Theorem

To perfectly reconstruct a signal with highest frequency $f_{\\max}$, the sampling rate must satisfy:

$$f_s \\geq 2 f_{\\max}$$

The **Nyquist rate** is the minimum sufficient rate:

$$f_{\\text{Nyquist}} = 2 f_{\\max}$$

**Example:** CD audio captures frequencies up to 22,050 Hz, so it uses $f_s = 44,100$ Hz — exactly the Nyquist rate.

### Aliasing

When a signal is sampled below the Nyquist rate, **aliasing** occurs: high-frequency components are "folded" into lower frequencies and become indistinguishable from them.

A frequency $f$ sampled at rate $f_s$ aliases to:

$$f_{\\text{alias}} = \\left|f - \\text{round}\\!\\left(\\frac{f}{f_s}\\right) \\cdot f_s\\right|$$

**Example:** A 1300 Hz tone sampled at 1000 Hz aliases to $|1300 - 1 \\cdot 1000| = 300$ Hz.

### Your Task

Implement:
- \`nyquist_rate(f_max)\` — returns $2 f_{\\max}$
- \`aliased_frequency(f, fs)\` — returns the aliased frequency when $f$ is sampled at $f_s$

\`\`\`python
def nyquist_rate(f_max):
    return 2 * f_max

def aliased_frequency(f, fs):
    return abs(f - round(f / fs) * fs)

print(nyquist_rate(22050))           # 44100
print(aliased_frequency(1300, 1000)) # 300
\`\`\``,

	starterCode: `def nyquist_rate(f_max):
    # Return the minimum sampling rate to capture f_max
    pass

def aliased_frequency(f, fs):
    # Return |f - round(f/fs)*fs|
    pass

print(nyquist_rate(1000))
print(aliased_frequency(1300, 1000))
`,

	solution: `def nyquist_rate(f_max):
    return 2 * f_max

def aliased_frequency(f, fs):
    return abs(f - round(f / fs) * fs)

print(nyquist_rate(1000))
print(aliased_frequency(1300, 1000))
`,

	tests: [
		{
			name: "nyquist_rate(1000) = 2000",
			code: `{{FUNC}}
print(nyquist_rate(1000))`,
			expected: "2000\n",
		},
		{
			name: "nyquist_rate(22050) = 44100 (CD audio)",
			code: `{{FUNC}}
print(nyquist_rate(22050))`,
			expected: "44100\n",
		},
		{
			name: "aliased_frequency(1300, 1000) = 300",
			code: `{{FUNC}}
print(aliased_frequency(1300, 1000))`,
			expected: "300\n",
		},
		{
			name: "aliased_frequency(700, 1000) = 300",
			code: `{{FUNC}}
print(aliased_frequency(700, 1000))`,
			expected: "300\n",
		},
		{
			name: "aliased_frequency(300, 1000) = 300 (no aliasing below Nyquist)",
			code: `{{FUNC}}
print(aliased_frequency(300, 1000))`,
			expected: "300\n",
		},
	],
};
