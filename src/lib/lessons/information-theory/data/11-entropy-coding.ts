import type { Lesson } from "../../types";

export const entropyCoding: Lesson = {
	id: "entropy-coding",
	title: "Coding Efficiency and Redundancy",
	chapterId: "coding",
	content: `## Coding Efficiency and Redundancy

Having computed the entropy $H$ and the average code length $\\bar{\\ell}$, we can evaluate how well a code performs.

### Coding Efficiency

**Coding efficiency** is the fraction of the average code length that is information (not waste):

$$\\eta = \\frac{H}{\\bar{\\ell}}$$

- $\\eta = 1.0$ — perfect: the code achieves the entropy bound
- $\\eta < 1.0$ — the code is longer than necessary

### Redundancy

**Redundancy** is the extra bits per symbol beyond the entropy:

$$R = \\bar{\\ell} - H$$

Shannon's source coding theorem guarantees $R < 1$ bit for Huffman codes.

### Compression Ratio

**Compression ratio** compares original and compressed sizes:

$$\\text{CR} = \\frac{\\text{original bits}}{\\text{compressed bits}}$$

- CR $> 1$ — compression achieved
- CR $= 1$ — no gain
- CR $< 1$ — expansion (the data was incompressible)

### Example

With $H = 1.75$ bits, $\\bar{\\ell} = 1.75$ bits:
$$\\eta = 1.75/1.75 = 1.0 \\quad R = 0.0 \\text{ bits}$$

With $H = 1.75$ bits, $\\bar{\\ell} = 2.0$ bits:
$$\\eta = 1.75/2.0 = 0.875 \\quad R = 0.25 \\text{ bits}$$

\`\`\`python
def coding_efficiency(H, avg_length):
    return H / avg_length

def redundancy(H, avg_length):
    return avg_length - H

def compression_ratio(original_bits, compressed_bits):
    return original_bits / compressed_bits

print(round(coding_efficiency(1.75, 2.0), 4))  # 0.875
print(redundancy(1.75, 2.0))                    # 0.25
print(compression_ratio(1000, 800))             # 1.25
\`\`\`

### Your Task

Implement:
- \`coding_efficiency(H, avg_length)\` — $H / \\bar{\\ell}$
- \`redundancy(H, avg_length)\` — $\\bar{\\ell} - H$
- \`compression_ratio(original_bits, compressed_bits)\` — ratio of sizes`,

	starterCode: `def coding_efficiency(H, avg_length):
    # Return H / avg_length
    pass

def redundancy(H, avg_length):
    # Return avg_length - H
    pass

def compression_ratio(original_bits, compressed_bits):
    # Return original_bits / compressed_bits
    pass

print(round(coding_efficiency(1.75, 2.0), 4))
print(redundancy(1.75, 2.0))
print(compression_ratio(1000, 800))
`,

	solution: `def coding_efficiency(H, avg_length):
    return H / avg_length

def redundancy(H, avg_length):
    return avg_length - H

def compression_ratio(original_bits, compressed_bits):
    return original_bits / compressed_bits

print(round(coding_efficiency(1.75, 2.0), 4))
print(redundancy(1.75, 2.0))
print(compression_ratio(1000, 800))
`,

	tests: [
		{
			name: "efficiency(1.75,2.0)=0.875, redundancy=0.25, ratio(1000,800)=1.25",
			expected: "0.875\n0.25\n1.25\n",
		},
		{
			name: "coding_efficiency(1.75, 1.75) = 1.0 (perfect)",
			code: `{{FUNC}}
print(coding_efficiency(1.75, 1.75))`,
			expected: "1.0\n",
		},
		{
			name: "redundancy(2.0, 2.0) = 0.0",
			code: `{{FUNC}}
print(redundancy(2.0, 2.0))`,
			expected: "0.0\n",
		},
		{
			name: "compression_ratio(100, 50) = 2.0",
			code: `{{FUNC}}
print(compression_ratio(100, 50))`,
			expected: "2.0\n",
		},
		{
			name: "round(compression_ratio(1000, 700), 4) = 1.4286",
			code: `{{FUNC}}
print(round(compression_ratio(1000, 700), 4))`,
			expected: "1.4286\n",
		},
	],
};
