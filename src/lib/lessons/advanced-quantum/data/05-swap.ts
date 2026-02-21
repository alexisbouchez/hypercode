import type { Lesson } from "../../types";

export const swapGate: Lesson = {
	id: "swap",
	title: "The SWAP Gate",
	chapterId: "multi-qubit-gates",
	content: `## The SWAP Gate

The **SWAP gate** exchanges the states of two qubits:

$$\\text{SWAP}|01\\rangle = |10\\rangle \\qquad \\text{SWAP}|10\\rangle = |01\\rangle$$

It leaves $|00\\rangle$ and $|11\\rangle$ unchanged.

### Matrix Representation

$$\\text{SWAP} = \\begin{pmatrix} 1 & 0 & 0 & 0 \\\\ 0 & 0 & 1 & 0 \\\\ 0 & 1 & 0 & 0 \\\\ 0 & 0 & 0 & 1 \\end{pmatrix}$$

### State Vector Indexing

A 2-qubit system has 4 basis states indexed as $q_0 \\cdot 2 + q_1$:

| Index | State |
|-------|-------|
| 0 | $|00\\rangle$ |
| 1 | $|01\\rangle$ |
| 2 | $|10\\rangle$ |
| 3 | $|11\\rangle$ |

SWAP exchanges the amplitudes at **indices 1 and 2** — swapping $|01\\rangle \\leftrightarrow |10\\rangle$.

### Decomposition into CNOTs

The SWAP gate can be decomposed into three CNOT gates:

$$\\text{SWAP} = \\text{CNOT}_{01} \\cdot \\text{CNOT}_{10} \\cdot \\text{CNOT}_{01}$$

This decomposition is useful in hardware where direct qubit-qubit connectivity is limited.

### Self-Inverse

Like CNOT and Toffoli, SWAP is its own inverse:

$$\\text{SWAP}^2 = I$$

### Applications

- **Qubit routing**: moving quantum information between non-adjacent qubits in hardware
- **Quantum sorting networks**: sorting qubit registers
- **iSWAP**: a variant $\\text{iSWAP}|01\\rangle = i|10\\rangle$ used in superconducting processors

### Implementation

\`\`\`python
def swap_gate(state):
    result = list(state)
    result[1], result[2] = state[2], state[1]
    return result
\`\`\`

### Your Task

Implement \`swap_gate(state)\` for a 4-element 2-qubit state vector that swaps the amplitudes of $|01\\rangle$ (index 1) and $|10\\rangle$ (index 2).`,

	starterCode: `def swap_gate(state):
    # Swap amplitudes at indices 1 (|01>) and 2 (|10>)
    pass

# |01> should become |10>
result = swap_gate([0.0, 1.0, 0.0, 0.0])
print(result[2])
print(result[1])
`,

	solution: `def swap_gate(state):
    result = list(state)
    result[1], result[2] = state[2], state[1]
    return result

# |01> should become |10>
result = swap_gate([0.0, 1.0, 0.0, 0.0])
print(result[2])
print(result[1])
`,

	tests: [
		{
			name: "SWAP maps |01⟩ → |10⟩",
			expected: "1.0\n0.0\n",
		},
		{
			name: "SWAP maps |10⟩ → |01⟩",
			code: `{{FUNC}}
result = swap_gate([0.0, 0.0, 1.0, 0.0])
print(result[1])`,
			expected: "1.0\n",
		},
		{
			name: "SWAP leaves |00⟩ unchanged",
			code: `{{FUNC}}
result = swap_gate([1.0, 0.0, 0.0, 0.0])
print(result[0])`,
			expected: "1.0\n",
		},
		{
			name: "SWAP leaves |11⟩ unchanged",
			code: `{{FUNC}}
result = swap_gate([0.0, 0.0, 0.0, 1.0])
print(result[3])`,
			expected: "1.0\n",
		},
	],
};
