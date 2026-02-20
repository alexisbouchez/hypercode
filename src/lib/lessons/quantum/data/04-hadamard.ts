import type { Lesson } from "../../types";

export const hadamardGate: Lesson = {
	id: "hadamard",
	title: "The Hadamard Gate",
	chapterId: "gates",
	content: `## Creating Superposition

The **Hadamard gate** (H) is the most fundamental quantum gate. It transforms basis states into equal superpositions:

- H|0⟩ = (|0⟩ + |1⟩) / √2 = |+⟩
- H|1⟩ = (|0⟩ - |1⟩) / √2 = |−⟩

As a matrix operation on [α, β]:

\`\`\`
H[α, β] = [(α + β)/√2, (α - β)/√2]
\`\`\`

\`\`\`python
import math

def hadamard(state):
    s = 1 / math.sqrt(2)
    a, b = state[0], state[1]
    return [s * (a + b), s * (a - b)]

zero = [1.0, 0.0]
plus = hadamard(zero)          # |+⟩
print(round(plus[0], 4))       # 0.7071
print(round(plus[1], 4))       # 0.7071
\`\`\`

A key property: **H applied twice is the identity** — H is its own inverse.

\`\`\`python
original = hadamard(hadamard(zero))
print(round(original[0], 4))   # 1.0
print(round(original[1], 4))   # 0.0
\`\`\`

### Your Task

Implement the Hadamard gate.`,

	starterCode: `import math

def hadamard(state):
    # Apply the Hadamard gate: [(a+b)/sqrt(2), (a-b)/sqrt(2)]
    pass

zero = [1.0, 0.0]
one = [0.0, 1.0]

plus = hadamard(zero)
minus = hadamard(one)

print(round(plus[0], 4))
print(round(plus[1], 4))
print(round(minus[0], 4))
print(round(minus[1], 4))

original = hadamard(hadamard(zero))
print(round(original[0], 4))
print(round(original[1], 4))
`,

	solution: `import math

def hadamard(state):
    s = 1 / math.sqrt(2)
    a, b = state[0], state[1]
    return [s * (a + b), s * (a - b)]

zero = [1.0, 0.0]
one = [0.0, 1.0]

plus = hadamard(zero)
minus = hadamard(one)

print(round(plus[0], 4))
print(round(plus[1], 4))
print(round(minus[0], 4))
print(round(minus[1], 4))

original = hadamard(hadamard(zero))
print(round(original[0], 4))
print(round(original[1], 4))
`,

	tests: [
		{
			name: "H|0⟩ = |+⟩ (equal superposition)",
			code: `{{FUNC}}
plus = hadamard([1.0, 0.0])
print(round(plus[0], 4))
print(round(plus[1], 4))`,
			expected: "0.7071\n0.7071\n",
		},
		{
			name: "H|1⟩ = |−⟩ (with minus sign)",
			code: `{{FUNC}}
minus = hadamard([0.0, 1.0])
print(round(minus[0], 4))
print(round(minus[1], 4))`,
			expected: "0.7071\n-0.7071\n",
		},
		{
			name: "H applied twice is identity",
			code: `{{FUNC}}
original = hadamard(hadamard([1.0, 0.0]))
print(round(original[0], 4))
print(round(original[1], 4))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "H preserves normalization",
			code: `{{FUNC}}
import math
result = hadamard([1.0, 0.0])
n = math.sqrt(result[0]**2 + result[1]**2)
print(round(n, 4))`,
			expected: "1.0\n",
		},
	],
};
