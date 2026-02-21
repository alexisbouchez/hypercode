import type { Lesson } from "../../types";

export const quantization: Lesson = {
	id: "quantization",
	title: "Quantization",
	chapterId: "fundamentals",
	content: `## Quantization

After sampling, the continuous amplitude values must be represented by a finite set of discrete levels — this is **quantization**. The number of bits $n$ determines how many levels are available:

$$\\text{levels} = 2^n$$

### Uniform Quantization

For a signal normalized to $[-1, 1]$, the step size is:

$$\\Delta = \\frac{2}{2^n}$$

The quantized value maps input $x$ to the midpoint of its quantization bin:

$$x_q = \\left(\\lfloor (x+1) / \\Delta \\rfloor + 0.5\\right) \\cdot \\Delta - 1$$

Values outside $[-1, 1]$ are clamped.

### Quantization SNR

The theoretical signal-to-noise ratio for $n$-bit quantization is:

$$\\text{SNR}_{\\text{dB}} = 6.02 n + 1.76 \\quad \\text{dB}$$

Each additional bit adds roughly 6 dB of dynamic range. A 16-bit audio CD achieves $\\approx 98$ dB SNR.

### Example

With $n = 2$ bits ($4$ levels, $\\Delta = 0.5$):
- Input $0.0 \\to$ bin index $\\lfloor 2.0 / 0.5 \\rfloor = 4$, clamped to $3$... Actually bin $\\lfloor 1.0 / 0.5 \\rfloor = 2$, midpoint $(2.5)\\cdot 0.5 - 1 = 0.25$

### Your Task

Implement:
- \`quantize(x, n_bits)\` — clamp $x$ to $[-1,1]$, quantize to $2^{n\\_bits}$ levels, return midpoint of bin
- \`quantization_snr_db(n_bits)\` — returns $6.02 n + 1.76$

\`\`\`python
import math

def quantize(x, n_bits):
    x = max(-1.0, min(1.0, x))
    levels = 2 ** n_bits
    step = 2.0 / levels
    q = math.floor((x + 1.0) / step)
    q = min(q, levels - 1)
    return (q + 0.5) * step - 1.0

def quantization_snr_db(n_bits):
    return 6.02 * n_bits + 1.76

print(quantize(0.0, 2))                    # 0.25
print(round(quantization_snr_db(8), 4))    # 49.92
\`\`\``,

	starterCode: `import math

def quantize(x, n_bits):
    # Clamp to [-1, 1], quantize to 2^n_bits levels, return bin midpoint
    pass

def quantization_snr_db(n_bits):
    # Return 6.02 * n_bits + 1.76
    pass

print(quantize(0.0, 2))
print(round(quantization_snr_db(8), 4))
`,

	solution: `import math

def quantize(x, n_bits):
    x = max(-1.0, min(1.0, x))
    levels = 2 ** n_bits
    step = 2.0 / levels
    q = math.floor((x + 1.0) / step)
    q = min(q, levels - 1)
    return (q + 0.5) * step - 1.0

def quantization_snr_db(n_bits):
    return 6.02 * n_bits + 1.76

print(quantize(0.0, 2))
print(round(quantization_snr_db(8), 4))
`,

	tests: [
		{
			name: "quantize(0.0, 2) = 0.25",
			code: `{{FUNC}}
print(quantize(0.0, 2))`,
			expected: "0.25\n",
		},
		{
			name: "quantize(1.0, 2) = 0.75 (clamp at max level)",
			code: `{{FUNC}}
print(quantize(1.0, 2))`,
			expected: "0.75\n",
		},
		{
			name: "quantize(-1.0, 2) = -0.75 (min level)",
			code: `{{FUNC}}
print(quantize(-1.0, 2))`,
			expected: "-0.75\n",
		},
		{
			name: "quantization_snr_db(8) = 49.92",
			code: `{{FUNC}}
print(round(quantization_snr_db(8), 4))`,
			expected: "49.92\n",
		},
		{
			name: "quantization_snr_db(16) = 98.08",
			code: `{{FUNC}}
print(round(quantization_snr_db(16), 4))`,
			expected: "98.08\n",
		},
	],
};
