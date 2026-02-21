import type { Lesson } from "../../types";

export const twoQubitStates: Lesson = {
	id: "two-qubit-states",
	title: "Two-Qubit States",
	chapterId: "multi-qubit",
	content: `## Combining Qubits

To build quantum systems, we combine multiple qubits using the **tensor product** ($\otimes$). Two qubits $[\alpha, \beta]$ and $[\gamma, \delta]$ combine into a 4-element state:

$$[\alpha, \beta] \otimes [\gamma, \delta] = [\alpha\gamma,\ \alpha\delta,\ \beta\gamma,\ \beta\delta]$$

The four elements represent amplitudes for the four basis states:
- Index 0: $|00\rangle$ (first=0, second=0)
- Index 1: $|01\rangle$ (first=0, second=1)
- Index 2: $|10\rangle$ (first=1, second=0)
- Index 3: $|11\rangle$ (first=1, second=1)

\`\`\`python
def tensor_product(q1, q2):
    return [
        q1[0] * q2[0],   # |00⟩
        q1[0] * q2[1],   # |01⟩
        q1[1] * q2[0],   # |10⟩
        q1[1] * q2[1],   # |11⟩
    ]

zero = [1.0, 0.0]
one  = [0.0, 1.0]

print(tensor_product(zero, zero))  # [1.0, 0.0, 0.0, 0.0] — |00⟩
print(tensor_product(one,  one))   # [0.0, 0.0, 0.0, 1.0] — |11⟩
\`\`\`

### Your Task

Implement \`tensor_product(q1, q2)\` that combines two single-qubit states.`,

	starterCode: `def tensor_product(q1, q2):
    # Return [q1[0]*q2[0], q1[0]*q2[1], q1[1]*q2[0], q1[1]*q2[1]]
    pass

zero = [1.0, 0.0]
one  = [0.0, 1.0]

print(tensor_product(zero, zero))
print(tensor_product(zero, one))
print(tensor_product(one,  zero))
print(tensor_product(one,  one))
`,

	solution: `def tensor_product(q1, q2):
    return [
        q1[0] * q2[0],
        q1[0] * q2[1],
        q1[1] * q2[0],
        q1[1] * q2[1],
    ]

zero = [1.0, 0.0]
one  = [0.0, 1.0]

print(tensor_product(zero, zero))
print(tensor_product(zero, one))
print(tensor_product(one,  zero))
print(tensor_product(one,  one))
`,

	tests: [
		{
			name: "|0⟩⊗|0⟩ = |00⟩",
			code: `{{FUNC}}
print(tensor_product([1.0, 0.0], [1.0, 0.0]))`,
			expected: "[1.0, 0.0, 0.0, 0.0]\n",
		},
		{
			name: "|0⟩⊗|1⟩ = |01⟩",
			code: `{{FUNC}}
print(tensor_product([1.0, 0.0], [0.0, 1.0]))`,
			expected: "[0.0, 1.0, 0.0, 0.0]\n",
		},
		{
			name: "|1⟩⊗|0⟩ = |10⟩",
			code: `{{FUNC}}
print(tensor_product([0.0, 1.0], [1.0, 0.0]))`,
			expected: "[0.0, 0.0, 1.0, 0.0]\n",
		},
		{
			name: "|1⟩⊗|1⟩ = |11⟩",
			code: `{{FUNC}}
print(tensor_product([0.0, 1.0], [0.0, 1.0]))`,
			expected: "[0.0, 0.0, 0.0, 1.0]\n",
		},
	],
};
