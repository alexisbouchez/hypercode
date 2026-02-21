import type { Lesson } from "../../types";

export const channelCapacity: Lesson = {
	id: "channel-capacity",
	title: "Channel Capacity",
	chapterId: "channel",
	content: `## Channel Capacity

**Shannon's channel capacity** is the maximum rate at which information can be transmitted reliably over a noisy channel:

$$C = \\max_{p(X)} I(X; Y) \\text{ bits/channel use}$$

### Binary Symmetric Channel (BSC)

The simplest noisy channel flips each bit independently with probability $p$:

$$\\text{BSC}(p): \\begin{cases} P(Y=0|X=0) = 1-p \\\\ P(Y=1|X=0) = p \\\\ P(Y=0|X=1) = p \\\\ P(Y=1|X=1) = 1-p \\end{cases}$$

### BSC Capacity

The maximum MI over BSC($p$) is achieved by the **uniform input distribution**:

$$C_{\\text{BSC}}(p) = 1 - H_b(p)$$

where $H_b(p)$ is the **binary entropy function**:

$$H_b(p) = -p \\log_2 p - (1-p) \\log_2(1-p)$$

### Special Cases

| Error probability | Capacity |
|---|---|
| $p = 0$ (noiseless) | $C = 1$ bit |
| $p = 0.5$ (pure noise) | $C = 0$ bits |
| $p = 1$ (inverted) | $C = 1$ bit (just flip output) |

\`\`\`python
import math

def binary_entropy(p):
    if p <= 0 or p >= 1:
        return 0.0
    return -p * math.log2(p) - (1-p) * math.log2(1-p)

def binary_channel_capacity(p_error):
    return 1 - binary_entropy(p_error)

print(binary_channel_capacity(0.0))   # 1.0
print(binary_channel_capacity(0.5))   # 0.0
print(round(binary_channel_capacity(0.1), 4))  # 0.531
\`\`\`

### Your Task

Implement:
- \`binary_entropy(p)\` — $H_b(p)$; return $0.0$ for $p \\leq 0$ or $p \\geq 1$
- \`binary_channel_capacity(p_error)\` — $1 - H_b(p)$
- \`bsc_capacity(p)\` — alias for \`binary_channel_capacity\``,

	starterCode: `import math

def binary_entropy(p):
    # Return 0.0 for p <= 0 or p >= 1
    # Otherwise: -p*log2(p) - (1-p)*log2(1-p)
    pass

def binary_channel_capacity(p_error):
    # 1 - binary_entropy(p_error)
    pass

def bsc_capacity(p):
    # Alias for binary_channel_capacity
    pass

print(binary_channel_capacity(0.0))
print(binary_channel_capacity(0.5))
print(round(bsc_capacity(0.1), 4))
`,

	solution: `import math

def binary_entropy(p):
    if p <= 0 or p >= 1:
        return 0.0
    return -p * math.log2(p) - (1-p) * math.log2(1-p)

def binary_channel_capacity(p_error):
    return 1 - binary_entropy(p_error)

def bsc_capacity(p):
    return binary_channel_capacity(p)

print(binary_channel_capacity(0.0))
print(binary_channel_capacity(0.5))
print(round(bsc_capacity(0.1), 4))
`,

	tests: [
		{
			name: "capacity(0.0)=1.0, capacity(0.5)=0.0, bsc(0.1)=0.531",
			expected: "1.0\n0.0\n0.531\n",
		},
		{
			name: "binary_channel_capacity(1.0) = 1.0 (inverted channel)",
			code: `{{FUNC}}
print(binary_channel_capacity(1.0))`,
			expected: "1.0\n",
		},
		{
			name: "binary_entropy(0.5) = 1.0 (maximum binary entropy)",
			code: `{{FUNC}}
print(binary_entropy(0.5))`,
			expected: "1.0\n",
		},
		{
			name: "binary_entropy(0.0) = 0.0",
			code: `{{FUNC}}
print(binary_entropy(0.0))`,
			expected: "0.0\n",
		},
		{
			name: "round(bsc_capacity(0.3), 4) = 0.1187",
			code: `{{FUNC}}
print(round(bsc_capacity(0.3), 4))`,
			expected: "0.1187\n",
		},
	],
};
