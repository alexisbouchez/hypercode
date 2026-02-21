import type { Lesson } from "../../types";

export const superdenseCoding: Lesson = {
	id: "superdense-coding",
	title: "Superdense Coding",
	chapterId: "protocols",
	content: `## Superdense Coding

**Superdense coding** is the quantum-classical dual of teleportation: where teleportation sends 1 qubit using 2 classical bits, superdense coding sends **2 classical bits using only 1 qubit** — provided Alice and Bob share a pre-established Bell pair.

This result is remarkable: no classical protocol can transmit 2 bits of information by sending a single bit. Entanglement is the resource that makes it possible.

### The Bell Pair

Alice and Bob start by sharing the Bell state:

$$|\\Phi^+\\rangle = \\frac{|00\\rangle + |11\\rangle}{\\sqrt{2}}$$

Alice holds qubit 0 (the first qubit); Bob holds qubit 1.

### Encoding

Alice encodes two classical bits $(b_0, b_1)$ by applying a gate to her qubit only:

| $(b_0,\\, b_1)$ | Alice's gate | Resulting shared state |
|---|---|---|
| $(0, 0)$ | $I$ | $\\frac{\\|00\\rangle + \\|11\\rangle}{\\sqrt{2}} = |\\Phi^+\\rangle$ |
| $(0, 1)$ | $X$ | $\\frac{\\|10\\rangle + \\|01\\rangle}{\\sqrt{2}} = |\\Psi^+\\rangle$ |
| $(1, 0)$ | $Z$ | $\\frac{\\|00\\rangle - \\|11\\rangle}{\\sqrt{2}} = |\\Phi^-\\rangle$ |
| $(1, 1)$ | $XZ$ | $\\frac{\\|10\\rangle - \\|01\\rangle}{\\sqrt{2}} = |\\Psi^-\\rangle$ |

The four resulting states are the four **Bell states** — an orthonormal basis.

### Decoding

Bob receives Alice's qubit and applies:
1. **CNOT** (Alice's qubit as control, Bob's as target)
2. **Hadamard** on Alice's qubit

This uncomputes the Bell circuit and leaves the two qubits in one of $|00\\rangle$, $|01\\rangle$, $|10\\rangle$, $|11\\rangle$, which Bob measures directly.

### State-Vector Convention

Index the 2-qubit computational basis as:
$$[|00\\rangle,\\; |01\\rangle,\\; |10\\rangle,\\; |11\\rangle]$$

so \`state[i]\` is the amplitude of the basis state whose binary representation is $i$.

### Verifying the Decode Circuit

For each encoded Bell state, CNOT maps $|10\\rangle \\to |11\\rangle$ and $|11\\rangle \\to |10\\rangle$ (swaps entries 2 and 3), then $H$ on qubit 0 maps:
$$\\frac{|0x\\rangle \\pm |1x\\rangle}{\\sqrt{2}} \\to |0x\\rangle \\text{ or } |1x\\rangle$$

You can verify all four cases in the solution walkthrough above.

### Your Task

Implement two functions:

1. \`encode_superdense(b0, b1)\` — return the 4-element complex state vector after Alice applies her encoding gate to $|\\Phi^+\\rangle$.
2. \`decode_superdense(state)\` — apply the CNOT + H decode circuit and return the recovered bits as a tuple \`(b0, b1)\`.`,

	starterCode: `import math

def encode_superdense(b0, b1):
    # Start from |Φ+⟩ and apply Alice's gate based on (b0, b1)
    # Return 4-element list [amp_00, amp_01, amp_10, amp_11]
    pass

def decode_superdense(state):
    # Apply CNOT (swap indices 2 and 3), then H on qubit 0
    # Return (b0, b1) from the dominant basis state
    pass

b0, b1 = decode_superdense(encode_superdense(0, 0))
print(b0, b1)
`,

	solution: `import math

def encode_superdense(b0, b1):
    sq2 = 1 / math.sqrt(2)
    if b0 == 0 and b1 == 0:
        return [sq2, 0, 0, sq2]
    elif b0 == 0 and b1 == 1:
        return [0, sq2, sq2, 0]
    elif b0 == 1 and b1 == 0:
        return [sq2, 0, 0, -sq2]
    else:
        return [0, sq2, -sq2, 0]

def decode_superdense(state):
    # CNOT: swap amplitudes of |10⟩ and |11⟩ (indices 2 and 3)
    after_cnot = [state[0], state[1], state[3], state[2]]
    # H on qubit 0: mix (|0x⟩, |1x⟩) pairs
    sq2 = 1 / math.sqrt(2)
    after_h = [
        sq2 * (after_cnot[0] + after_cnot[2]),
        sq2 * (after_cnot[1] + after_cnot[3]),
        sq2 * (after_cnot[0] - after_cnot[2]),
        sq2 * (after_cnot[1] - after_cnot[3]),
    ]
    probs = [abs(x) ** 2 for x in after_h]
    best = max(range(4), key=lambda i: probs[i])
    b0 = (best >> 1) & 1
    b1 = best & 1
    return (b0, b1)

b0, b1 = decode_superdense(encode_superdense(0, 0))
print(b0, b1)
`,

	tests: [
		{
			name: "encode/decode (0,0) → '0 0'",
			expected: "0 0\n",
		},
		{
			name: "encode/decode (0,1) → '0 1'",
			code: `{{FUNC}}
b0, b1 = decode_superdense(encode_superdense(0, 1))
print(b0, b1)`,
			expected: "0 1\n",
		},
		{
			name: "encode/decode (1,0) → '1 0'",
			code: `{{FUNC}}
b0, b1 = decode_superdense(encode_superdense(1, 0))
print(b0, b1)`,
			expected: "1 0\n",
		},
		{
			name: "encode/decode (1,1) → '1 1'",
			code: `{{FUNC}}
b0, b1 = decode_superdense(encode_superdense(1, 1))
print(b0, b1)`,
			expected: "1 1\n",
		},
	],
};
