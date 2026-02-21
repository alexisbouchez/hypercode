import type { Lesson } from "../../types";

export const signalPower: Lesson = {
	id: "signal-power",
	title: "Signal Power and Energy",
	chapterId: "fundamentals",
	content: `## Signal Power and Energy

Two fundamental quantities describe the "strength" of a signal.

### Signal Energy

The **energy** of a discrete signal $x[n]$ over $N$ samples is the sum of squared values:

$$E = \\sum_{n=0}^{N-1} x[n]^2$$

### Signal Power

The **average power** is energy normalized by the number of samples:

$$P = \\frac{1}{N} \\sum_{n=0}^{N-1} x[n]^2$$

### RMS (Root Mean Square)

The **RMS** value is the square root of average power — it represents the effective amplitude:

$$\\text{RMS} = \\sqrt{\\frac{1}{N} \\sum_{n=0}^{N-1} x[n]^2} = \\sqrt{P}$$

For a pure sinusoid $x[n] = A\\sin(\\theta)$, the RMS is $A/\\sqrt{2} \\approx 0.707 A$.

### Example

For the signal $[1, -1, 1, -1]$ (alternating $\\pm 1$):
- Energy $= 1 + 1 + 1 + 1 = 4$
- Power $= 4/4 = 1.0$
- RMS $= \\sqrt{1.0} = 1.0$

For $[0, 1, 0, -1]$:
- Power $= (0+1+0+1)/4 = 0.5$
- RMS $= \\sqrt{0.5} \\approx 0.7071$

### Your Task

Implement:
- \`signal_power(samples)\` — returns $\\frac{1}{N}\\sum x[n]^2$
- \`signal_energy(samples)\` — returns $\\sum x[n]^2$
- \`rms(samples)\` — returns $\\sqrt{\\text{power}}$

\`\`\`python
import math

def signal_power(samples):
    return sum(x**2 for x in samples) / len(samples)

def signal_energy(samples):
    return sum(x**2 for x in samples)

def rms(samples):
    return math.sqrt(signal_power(samples))
\`\`\``,

	starterCode: `import math

def signal_power(samples):
    # Return (1/N) * sum of x^2
    pass

def signal_energy(samples):
    # Return sum of x^2
    pass

def rms(samples):
    # Return sqrt(signal_power(samples))
    pass

print(signal_power([1.0, -1.0, 1.0, -1.0]))
print(signal_energy([1.0, 2.0, 3.0]))
print(round(rms([0.0, 1.0, 0.0, -1.0]), 4))
`,

	solution: `import math

def signal_power(samples):
    return sum(x**2 for x in samples) / len(samples)

def signal_energy(samples):
    return sum(x**2 for x in samples)

def rms(samples):
    return math.sqrt(signal_power(samples))

print(signal_power([1.0, -1.0, 1.0, -1.0]))
print(signal_energy([1.0, 2.0, 3.0]))
print(round(rms([0.0, 1.0, 0.0, -1.0]), 4))
`,

	tests: [
		{
			name: "signal_power([1.0, -1.0, 1.0, -1.0]) = 1.0",
			code: `{{FUNC}}
print(signal_power([1.0, -1.0, 1.0, -1.0]))`,
			expected: "1.0\n",
		},
		{
			name: "signal_energy([1.0, 2.0, 3.0]) = 14.0",
			code: `{{FUNC}}
print(signal_energy([1.0, 2.0, 3.0]))`,
			expected: "14.0\n",
		},
		{
			name: "rms([0.0, 1.0, 0.0, -1.0]) = 0.7071",
			code: `{{FUNC}}
print(round(rms([0.0, 1.0, 0.0, -1.0]), 4))`,
			expected: "0.7071\n",
		},
		{
			name: "signal_power([0.0, 0.0, 0.0]) = 0.0",
			code: `{{FUNC}}
print(signal_power([0.0, 0.0, 0.0]))`,
			expected: "0.0\n",
		},
		{
			name: "rms([1.0, 1.0, 1.0, 1.0]) = 1.0",
			code: `{{FUNC}}
print(round(rms([1.0, 1.0, 1.0, 1.0]), 4))`,
			expected: "1.0\n",
		},
	],
};
