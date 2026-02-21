import type { Lesson } from "../../types";

export const toffoliGate: Lesson = {
	id: "toffoli",
	title: "The Toffoli Gate (CCNOT)",
	chapterId: "multi-qubit-gates",
	content: `## The Toffoli Gate

The **Toffoli gate** (also called CCNOT or controlled-controlled-NOT) is a universal reversible 3-qubit gate. It flips the **target** qubit if and only if both **control** qubits are in state $|1\\rangle$.

### Truth Table

| $q_0$ | $q_1$ | $q_2$ (target) | Output $q_2$ |
|--------|--------|-----------------|--------------|
| 0 | 0 | 0 | 0 |
| 0 | 0 | 1 | 1 |
| 0 | 1 | 0 | 0 |
| 0 | 1 | 1 | 1 |
| 1 | 0 | 0 | 0 |
| 1 | 0 | 1 | 1 |
| 1 | 1 | 0 | **1** ← flipped |
| 1 | 1 | 1 | **0** ← flipped |

### State Vector Representation

A 3-qubit system has $2^3 = 8$ basis states. We index them as:

$$\\text{index} = q_0 \\cdot 4 + q_1 \\cdot 2 + q_2$$

So the basis states map to indices:

| Index | State |
|-------|-------|
| 0 | $|000\\rangle$ |
| 1 | $|001\\rangle$ |
| 2 | $|010\\rangle$ |
| 3 | $|011\\rangle$ |
| 4 | $|100\\rangle$ |
| 5 | $|101\\rangle$ |
| 6 | $|110\\rangle$ |
| 7 | $|111\\rangle$ |

The Toffoli gate **swaps amplitudes at indices 6 and 7** — it exchanges $|110\\rangle \\leftrightarrow |111\\rangle$.

### Universality

The Toffoli gate is **classically universal**: any Boolean function can be computed reversibly using Toffoli gates. Combined with the Hadamard gate, it is also **quantum universal**.

### Self-Inverse

Like CNOT, the Toffoli gate is its own inverse: applying it twice returns to the original state.

$$\\text{Toffoli}^2 = I$$

### Implementation

\`\`\`python
def toffoli(state):
    result = list(state)
    result[6], result[7] = state[7], state[6]
    return result
\`\`\`

### Your Task

Implement \`toffoli(state)\` for an 8-element state vector that swaps the amplitudes of $|110\\rangle$ and $|111\\rangle$.`,

	starterCode: `def toffoli(state):
    # Swap amplitudes at indices 6 (|110>) and 7 (|111>)
    pass

# |110> should become |111>
result = toffoli([0,0,0,0,0,0,1,0])
print(result[7])
print(result[6])
`,

	solution: `def toffoli(state):
    result = list(state)
    result[6], result[7] = state[7], state[6]
    return result

# |110> should become |111>
result = toffoli([0,0,0,0,0,0,1,0])
print(result[7])
print(result[6])
`,

	tests: [
		{
			name: "Toffoli maps |110⟩ → |111⟩",
			expected: "1\n0\n",
		},
		{
			name: "Toffoli maps |111⟩ → |110⟩",
			code: `{{FUNC}}
result = toffoli([0,0,0,0,0,0,0,1])
print(result[6])`,
			expected: "1\n",
		},
		{
			name: "Toffoli leaves |010⟩ unchanged",
			code: `{{FUNC}}
result = toffoli([0,0,1,0,0,0,0,0])
print(result[2])`,
			expected: "1\n",
		},
		{
			name: "Toffoli is self-inverse",
			code: `{{FUNC}}
result = toffoli(toffoli([0,0,0,0,0,0,1,0]))
print(result[6])`,
			expected: "1\n",
		},
	],
};
