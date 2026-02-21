import type { Lesson } from "../../types";

export const errorCorrection: Lesson = {
	id: "error-correction",
	title: "Quantum Error Correction",
	chapterId: "algorithms",
	content: `## Protecting Quantum Information

Quantum computers are fragile — qubits interact with their environment, causing **decoherence** and bit-flip errors. Quantum error correction protects information by encoding a single logical qubit into multiple physical qubits.

The simplest scheme is the **3-qubit bit-flip code**:
- Logical $|0\rangle$ is encoded as physical $|000\rangle$
- Logical $|1\rangle$ is encoded as physical $|111\rangle$

If one of the three physical qubits flips, we can detect and correct the error by **majority vote** — two qubits always agree on the original value.

\`\`\`python
def encode(bit):
    return [bit, bit, bit]

def introduce_error(state, position):
    result = state[:]
    result[position] = 1 - result[position]
    return result

def decode_and_correct(state):
    return 1 if sum(state) >= 2 else 0

encoded = encode(0)             # [0, 0, 0]
with_error = introduce_error(encoded, 1)  # [0, 1, 0]
corrected = decode_and_correct(with_error)  # 0 — correct!
\`\`\`

The code can correct any **single-bit error** but fails if two or more qubits flip.

### Your Task

Implement \`encode(bit)\`, \`introduce_error(state, position)\`, and \`decode_and_correct(state)\`.`,

	starterCode: `def encode(bit):
    # Return [bit, bit, bit]
    pass

def introduce_error(state, position):
    # Flip the bit at the given position
    pass

def decode_and_correct(state):
    # Return 1 if majority is 1, else 0
    pass

encoded_0 = encode(0)
encoded_1 = encode(1)
print(encoded_0)
print(encoded_1)
print(decode_and_correct(encoded_0))
print(decode_and_correct(encoded_1))

with_error = introduce_error(encode(0), 0)
print(with_error)
print(decode_and_correct(with_error))
`,

	solution: `def encode(bit):
    return [bit, bit, bit]

def introduce_error(state, position):
    result = state[:]
    result[position] = 1 - result[position]
    return result

def decode_and_correct(state):
    return 1 if sum(state) >= 2 else 0

encoded_0 = encode(0)
encoded_1 = encode(1)
print(encoded_0)
print(encoded_1)
print(decode_and_correct(encoded_0))
print(decode_and_correct(encoded_1))

with_error = introduce_error(encode(0), 0)
print(with_error)
print(decode_and_correct(with_error))
`,

	tests: [
		{
			name: "encode(0) returns [0, 0, 0]",
			code: `{{FUNC}}
print(encode(0))`,
			expected: "[0, 0, 0]\n",
		},
		{
			name: "encode(1) returns [1, 1, 1]",
			code: `{{FUNC}}
print(encode(1))`,
			expected: "[1, 1, 1]\n",
		},
		{
			name: "single error on bit 0 is corrected",
			code: `{{FUNC}}
corrupted = introduce_error(encode(0), 0)
print(corrupted)
print(decode_and_correct(corrupted))`,
			expected: "[1, 0, 0]\n0\n",
		},
		{
			name: "single error on bit 2 is corrected",
			code: `{{FUNC}}
corrupted = introduce_error(encode(1), 2)
print(corrupted)
print(decode_and_correct(corrupted))`,
			expected: "[1, 1, 0]\n1\n",
		},
	],
};
