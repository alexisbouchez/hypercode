import type { Lesson } from "../../types";

export const selfInformation: Lesson = {
	id: "self-information",
	title: "Self-Information",
	chapterId: "entropy",
	content: `## Self-Information

**Self-information** (also called **surprisal**) quantifies how much information is gained when you learn that an event with probability $p$ has occurred.

$$I(p) = -\\log_2(p) \\text{ bits}$$

### Intuition

- A **certain event** ($p = 1$) carries **0 bits** of information — you already knew it would happen.
- A **coin flip** ($p = 0.5$) carries **1 bit** — you need one yes/no question to resolve it.
- A **rare event** ($p = 0.125$) carries **3 bits** — it takes three yes/no questions to pin down.

### Log Base Choice

Using $\\log_2$ gives information in **bits**. Using $\\ln$ gives **nats**. This course uses bits throughout.

### Example

$$I(0.25) = -\\log_2(0.25) = -\\log_2(2^{-2}) = 2 \\text{ bits}$$

\`\`\`python
import math

def self_information(p):
    return -math.log2(p)

print(self_information(0.5))   # 1.0 bit
print(self_information(0.25))  # 2.0 bits
\`\`\`

### Your Task

Implement three equivalent functions:
- \`self_information(p)\` — returns $-\\log_2(p)$
- \`information_content(p)\` — same formula
- \`surprise(p)\` — same formula (different name used in some literature)

All three should return results in bits.`,

	starterCode: `import math

def self_information(p):
    # Return -log2(p) in bits
    pass

def information_content(p):
    # Same as self_information
    pass

def surprise(p):
    # Same as self_information
    pass

print(self_information(0.5))
print(self_information(0.25))
`,

	solution: `import math

def self_information(p):
    return -math.log2(p)

def information_content(p):
    return -math.log2(p)

def surprise(p):
    return -math.log2(p)

print(self_information(0.5))
print(self_information(0.25))
`,

	tests: [
		{
			name: "self_information(0.5) = 1.0, self_information(0.25) = 2.0",
			expected: "1.0\n2.0\n",
		},
		{
			name: "self_information(0.125) = 3.0 bits",
			code: `{{FUNC}}
print(self_information(0.125))`,
			expected: "3.0\n",
		},
		{
			name: "information_content(0.5) = 1.0",
			code: `{{FUNC}}
print(information_content(0.5))`,
			expected: "1.0\n",
		},
		{
			name: "surprise(0.25) = 2.0",
			code: `{{FUNC}}
print(surprise(0.25))`,
			expected: "2.0\n",
		},
		{
			name: "round(self_information(0.1), 4) = 3.3219",
			code: `{{FUNC}}
print(round(self_information(0.1), 4))`,
			expected: "3.3219\n",
		},
	],
};
