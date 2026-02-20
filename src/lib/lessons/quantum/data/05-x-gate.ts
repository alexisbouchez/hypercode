import type { Lesson } from "../../types";

export const xGate: Lesson = {
	id: "x-gate",
	title: "The X Gate",
	chapterId: "gates",
	content: `## The Quantum NOT Gate

The **Pauli-X gate** (X) is the quantum equivalent of the classical NOT gate. It flips the amplitudes:

- X|0⟩ = |1⟩
- X|1⟩ = |0⟩

On a general state [α, β], it simply swaps the two amplitudes:

\`\`\`
X[α, β] = [β, α]
\`\`\`

\`\`\`python
def x_gate(state):
    return [state[1], state[0]]

zero = [1.0, 0.0]
one = [0.0, 1.0]

print(x_gate(zero))  # [0.0, 1.0]  — flipped to |1⟩
print(x_gate(one))   # [1.0, 0.0]  — flipped to |0⟩
\`\`\`

Like the Hadamard gate, **X applied twice is the identity** — applying NOT twice returns to the original state.

On a superposition state [1/√2, 1/√2], X has no effect because both amplitudes are equal. On [1/√2, -1/√2], X flips the sign pattern.

### Your Task

Implement \`x_gate(state)\` that applies the Pauli-X gate.`,

	starterCode: `def x_gate(state):
    # Swap the two amplitudes
    pass

zero = [1.0, 0.0]
one = [0.0, 1.0]

print(x_gate(zero))
print(x_gate(one))
print(x_gate(x_gate(zero)) == zero)
print(x_gate(x_gate(one)) == one)
`,

	solution: `def x_gate(state):
    return [state[1], state[0]]

zero = [1.0, 0.0]
one = [0.0, 1.0]

print(x_gate(zero))
print(x_gate(one))
print(x_gate(x_gate(zero)) == zero)
print(x_gate(x_gate(one)) == one)
`,

	tests: [
		{
			name: "X|0⟩ = |1⟩",
			code: `{{FUNC}}
print(x_gate([1.0, 0.0]))`,
			expected: "[0.0, 1.0]\n",
		},
		{
			name: "X|1⟩ = |0⟩",
			code: `{{FUNC}}
print(x_gate([0.0, 1.0]))`,
			expected: "[1.0, 0.0]\n",
		},
		{
			name: "X applied twice is identity",
			code: `{{FUNC}}
print(x_gate(x_gate([1.0, 0.0])) == [1.0, 0.0])
print(x_gate(x_gate([0.0, 1.0])) == [0.0, 1.0])`,
			expected: "True\nTrue\n",
		},
		{
			name: "X swaps arbitrary amplitudes",
			code: `{{FUNC}}
result = x_gate([0.6, 0.8])
print(result[0])
print(result[1])`,
			expected: "0.8\n0.6\n",
		},
	],
};
