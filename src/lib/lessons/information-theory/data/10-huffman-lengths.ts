import type { Lesson } from "../../types";

export const huffmanLengths: Lesson = {
	id: "huffman-lengths",
	title: "Huffman Code Lengths",
	chapterId: "coding",
	content: `## Huffman Code Lengths

**Huffman coding** is a lossless data compression scheme that assigns shorter codes to more frequent symbols and longer codes to rarer ones.

### Optimal Code Length Formula

For a symbol with probability $p$, the **optimal code length** (in bits) is:

$$\\ell(p) = \\lceil -\\log_2 p \\rceil$$

A simplified but instructive greedy approximation uses the floor:

$$\\ell(p) = \\max(1, \\lfloor -\\log_2 p \\rfloor)$$

The minimum of 1 bit ensures every symbol gets at least one bit.

### Average Code Length

Once lengths are assigned, the **average code length** is:

$$\\bar{\\ell} = \\sum_i p_i \\cdot \\ell_i \\text{ bits/symbol}$$

### Shannon's Source Coding Theorem

For any uniquely decodable code:

$$H(X) \\leq \\bar{\\ell} < H(X) + 1$$

Huffman coding achieves this bound: the average length is within 1 bit of the entropy.

### Example

For probabilities $[0.5, 0.25, 0.125, 0.125]$:
- Lengths: $[\\lfloor 1 \\rfloor, \\lfloor 2 \\rfloor, \\lfloor 3 \\rfloor, \\lfloor 3 \\rfloor] = [1, 2, 3, 3]$
- Average: $0.5 \\cdot 1 + 0.25 \\cdot 2 + 0.125 \\cdot 3 + 0.125 \\cdot 3 = 1.75$ bits
- Entropy: $H = 1.75$ bits (exactly matched here!)

\`\`\`python
import math

def huffman_code_lengths(probs):
    return [max(1, math.floor(-math.log2(p))) for p in probs]

def average_code_length(probs, lengths):
    return sum(p * l for p, l in zip(probs, lengths))

probs = [0.5, 0.25, 0.125, 0.125]
lengths = huffman_code_lengths(probs)
print(lengths)                            # [1, 2, 3, 3]
print(average_code_length(probs, lengths)) # 1.75
\`\`\`

### Your Task

Implement:
- \`huffman_code_lengths(probs)\` — returns list of lengths using $\\max(1, \\lfloor -\\log_2 p \\rfloor)$
- \`average_code_length(probs, lengths)\` — returns $\\sum p_i \\ell_i$`,

	starterCode: `import math

def huffman_code_lengths(probs):
    # Return [max(1, floor(-log2(p))) for p in probs]
    pass

def average_code_length(probs, lengths):
    # Return sum(p * l for p, l in zip(probs, lengths))
    pass

probs = [0.5, 0.25, 0.125, 0.125]
lengths = huffman_code_lengths(probs)
print(lengths)
print(average_code_length(probs, lengths))
`,

	solution: `import math

def huffman_code_lengths(probs):
    return [max(1, math.floor(-math.log2(p))) for p in probs]

def average_code_length(probs, lengths):
    return sum(p * l for p, l in zip(probs, lengths))

probs = [0.5, 0.25, 0.125, 0.125]
lengths = huffman_code_lengths(probs)
print(lengths)
print(average_code_length(probs, lengths))
`,

	tests: [
		{
			name: "[0.5,0.25,0.125,0.125] -> lengths=[1,2,3,3], avg=1.75",
			expected: "[1, 2, 3, 3]\n1.75\n",
		},
		{
			name: "huffman_code_lengths([0.5, 0.5]) = [1, 1]",
			code: `{{FUNC}}
print(huffman_code_lengths([0.5, 0.5]))`,
			expected: "[1, 1]\n",
		},
		{
			name: "huffman_code_lengths([0.25]*4) = [2, 2, 2, 2]",
			code: `{{FUNC}}
print(huffman_code_lengths([0.25, 0.25, 0.25, 0.25]))`,
			expected: "[2, 2, 2, 2]\n",
		},
		{
			name: "average_code_length([0.5,0.5],[1,1]) = 1.0",
			code: `{{FUNC}}
print(average_code_length([0.5, 0.5], [1, 1]))`,
			expected: "1.0\n",
		},
		{
			name: "average_code_length([0.25]*4,[2,2,2,2]) = 2.0",
			code: `{{FUNC}}
print(average_code_length([0.25, 0.25, 0.25, 0.25], [2, 2, 2, 2]))`,
			expected: "2.0\n",
		},
	],
};
