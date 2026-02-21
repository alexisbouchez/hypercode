import type { Lesson } from "../../types";

export const qubits: Lesson = {
	id: "qubits",
	title: "Qubits",
	chapterId: "qubits",
	content: `## The Quantum Bit

A classical bit is either 0 or 1. A **qubit** (quantum bit) can be in a superposition of both states simultaneously — until it is measured.

We represent a qubit as a pair of amplitudes $[\alpha, \beta]$ where:

- $\alpha$ is the amplitude for the $|0\rangle$ state
- $\beta$ is the amplitude for the $|1\rangle$ state
- $|\alpha|^2 + |\beta|^2 = 1$ (the probabilities must sum to 1)

The two **basis states** are:

| State | Notation | Vector |
|-------|----------|--------|
| Zero  | $|0\rangle$ | [1.0, 0.0] |
| One   | $|1\rangle$ | [0.0, 1.0] |

\`\`\`python
def ket_zero():
    return [1.0, 0.0]  # |0⟩

def ket_one():
    return [0.0, 1.0]  # |1⟩

zero = ket_zero()
one = ket_one()
print(zero)    # [1.0, 0.0]
print(one)     # [0.0, 1.0]
\`\`\`

The alpha ($\alpha$) amplitude is at index 0 and beta ($\beta$) is at index 1.

### Your Task

Implement \`ket_zero()\` and \`ket_one()\` that return the two computational basis states as two-element lists of floats. Then implement \`amplitude_zero(state)\` and \`amplitude_one(state)\` that extract the respective amplitudes.`,

	starterCode: `def ket_zero():
    # Return the |0⟩ basis state as [alpha, beta]
    pass

def ket_one():
    # Return the |1⟩ basis state as [alpha, beta]
    pass

def amplitude_zero(state):
    # Return the alpha amplitude (index 0)
    pass

def amplitude_one(state):
    # Return the beta amplitude (index 1)
    pass

zero = ket_zero()
one = ket_one()
print(zero)
print(one)
print(amplitude_zero(zero))
print(amplitude_one(one))
`,

	solution: `def ket_zero():
    return [1.0, 0.0]

def ket_one():
    return [0.0, 1.0]

def amplitude_zero(state):
    return state[0]

def amplitude_one(state):
    return state[1]

zero = ket_zero()
one = ket_one()
print(zero)
print(one)
print(amplitude_zero(zero))
print(amplitude_one(one))
`,

	tests: [
		{
			name: "ket_zero returns [1.0, 0.0]",
			code: `{{FUNC}}
print(ket_zero())`,
			expected: "[1.0, 0.0]\n",
		},
		{
			name: "ket_one returns [0.0, 1.0]",
			code: `{{FUNC}}
print(ket_one())`,
			expected: "[0.0, 1.0]\n",
		},
		{
			name: "amplitude_zero extracts index 0",
			code: `{{FUNC}}
print(amplitude_zero([1.0, 0.0]))
print(amplitude_zero([0.0, 1.0]))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "amplitude_one extracts index 1",
			code: `{{FUNC}}
print(amplitude_one([1.0, 0.0]))
print(amplitude_one([0.0, 1.0]))`,
			expected: "0.0\n1.0\n",
		},
	],
};
