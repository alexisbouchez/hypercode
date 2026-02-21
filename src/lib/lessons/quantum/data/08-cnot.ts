import type { Lesson } from "../../types";

export const cnot: Lesson = {
	id: "cnot",
	title: "The CNOT Gate",
	chapterId: "multi-qubit",
	content: `## Controlled Operations

The **CNOT gate** (Controlled-NOT) is the most important two-qubit gate. It flips the second qubit (target) if and only if the first qubit (control) is $|1\rangle$:

| Input | Output |
|-------|--------|
| $|00\rangle$ | $|00\rangle$ |
| $|01\rangle$ | $|01\rangle$ |
| $|10\rangle$ | $|11\rangle$ |
| $|11\rangle$ | $|10\rangle$ |

In terms of the 4-element state vector $[\alpha_{00}, \alpha_{01}, \alpha_{10}, \alpha_{11}]$:

$$\text{CNOT}[\alpha_{00}, \alpha_{01}, \alpha_{10}, \alpha_{11}] = [\alpha_{00}, \alpha_{01}, \alpha_{11}, \alpha_{10}]$$

Indices 2 and 3 (the $|10\rangle$ and $|11\rangle$ amplitudes) are swapped.

\`\`\`python
def cnot(state):
    return [state[0], state[1], state[3], state[2]]

ket_10 = [0.0, 0.0, 1.0, 0.0]  # |10⟩
print(cnot(ket_10))  # [0.0, 0.0, 0.0, 1.0] — becomes |11⟩
\`\`\`

### Your Task

Implement \`cnot(state)\` for a 4-element two-qubit state.`,

	starterCode: `def cnot(state):
    # Swap indices 2 and 3 (the |10⟩ and |11⟩ amplitudes)
    pass

ket_00 = [1.0, 0.0, 0.0, 0.0]
ket_01 = [0.0, 1.0, 0.0, 0.0]
ket_10 = [0.0, 0.0, 1.0, 0.0]
ket_11 = [0.0, 0.0, 0.0, 1.0]

print(cnot(ket_00))
print(cnot(ket_01))
print(cnot(ket_10))
print(cnot(ket_11))
`,

	solution: `def cnot(state):
    return [state[0], state[1], state[3], state[2]]

ket_00 = [1.0, 0.0, 0.0, 0.0]
ket_01 = [0.0, 1.0, 0.0, 0.0]
ket_10 = [0.0, 0.0, 1.0, 0.0]
ket_11 = [0.0, 0.0, 0.0, 1.0]

print(cnot(ket_00))
print(cnot(ket_01))
print(cnot(ket_10))
print(cnot(ket_11))
`,

	tests: [
		{
			name: "CNOT|00⟩ = |00⟩ (control is 0, no flip)",
			code: `{{FUNC}}
print(cnot([1.0, 0.0, 0.0, 0.0]))`,
			expected: "[1.0, 0.0, 0.0, 0.0]\n",
		},
		{
			name: "CNOT|01⟩ = |01⟩ (control is 0, no flip)",
			code: `{{FUNC}}
print(cnot([0.0, 1.0, 0.0, 0.0]))`,
			expected: "[0.0, 1.0, 0.0, 0.0]\n",
		},
		{
			name: "CNOT|10⟩ = |11⟩ (control is 1, target flipped)",
			code: `{{FUNC}}
print(cnot([0.0, 0.0, 1.0, 0.0]))`,
			expected: "[0.0, 0.0, 0.0, 1.0]\n",
		},
		{
			name: "CNOT|11⟩ = |10⟩ (control is 1, target flipped)",
			code: `{{FUNC}}
print(cnot([0.0, 0.0, 0.0, 1.0]))`,
			expected: "[0.0, 0.0, 1.0, 0.0]\n",
		},
	],
};
