import type { Lesson } from "../../types";

export const qkd: Lesson = {
	id: "qkd",
	title: "Quantum Key Distribution",
	chapterId: "algorithms",
	content: `## Unbreakable Encryption

**Quantum Key Distribution (QKD)** uses quantum mechanics to share a secret key between two parties — with the guarantee that any eavesdropping is physically detectable.

The **BB84 protocol** (Bennett & Brassard, 1984) works as follows:

1. **Alice** prepares qubits in one of two bases:
   - **Z basis** (computational): 0 → |0⟩, 1 → |1⟩
   - **X basis** (diagonal): 0 → |+⟩, 1 → |−⟩

2. **Bob** measures each qubit in a randomly chosen basis.

3. **Sifting**: Alice and Bob publicly announce which bases they used. They keep only the results where their bases matched — these form the secret key.

When bases match, measurement is **deterministic**: Bob gets exactly what Alice sent. When they don't match, the result is random and discarded.

\`\`\`python
def sift_keys(alice_bits, alice_bases, bob_bases):
    alice_key, bob_key = [], []
    for a_bit, a_base, b_base in zip(alice_bits, alice_bases, bob_bases):
        if a_base == b_base:  # Bases matched!
            alice_key.append(a_bit)
            bob_key.append(a_bit)  # Deterministic when bases agree
    return alice_key, bob_key
\`\`\`

### Your Task

Implement \`sift_keys(alice_bits, alice_bases, bob_bases)\` that returns the matching key bits for Alice and Bob.`,

	starterCode: `def sift_keys(alice_bits, alice_bases, bob_bases):
    # For each position where a_base == b_base, add a_bit to both keys
    alice_key = []
    bob_key = []
    for a_bit, a_base, b_base in zip(alice_bits, alice_bases, bob_bases):
        pass  # your code here
    return alice_key, bob_key

alice_bits  = [0, 1, 0, 0, 1, 1, 0, 1]
alice_bases = [0, 0, 1, 0, 1, 0, 1, 1]
bob_bases   = [0, 1, 1, 0, 0, 0, 1, 1]

alice_key, bob_key = sift_keys(alice_bits, alice_bases, bob_bases)
print(len(alice_key))
print(alice_key)
print(bob_key)
print(alice_key == bob_key)
`,

	solution: `def sift_keys(alice_bits, alice_bases, bob_bases):
    alice_key = []
    bob_key = []
    for a_bit, a_base, b_base in zip(alice_bits, alice_bases, bob_bases):
        if a_base == b_base:
            alice_key.append(a_bit)
            bob_key.append(a_bit)
    return alice_key, bob_key

alice_bits  = [0, 1, 0, 0, 1, 1, 0, 1]
alice_bases = [0, 0, 1, 0, 1, 0, 1, 1]
bob_bases   = [0, 1, 1, 0, 0, 0, 1, 1]

alice_key, bob_key = sift_keys(alice_bits, alice_bases, bob_bases)
print(len(alice_key))
print(alice_key)
print(bob_key)
print(alice_key == bob_key)
`,

	tests: [
		{
			name: "sift_keys returns only matching-basis bits",
			code: `{{FUNC}}
alice_bits  = [0, 1, 0, 0, 1, 1, 0, 1]
alice_bases = [0, 0, 1, 0, 1, 0, 1, 1]
bob_bases   = [0, 1, 1, 0, 0, 0, 1, 1]
alice_key, _ = sift_keys(alice_bits, alice_bases, bob_bases)
print(len(alice_key))`,
			expected: "6\n",
		},
		{
			name: "sifted key contains correct bits",
			code: `{{FUNC}}
alice_bits  = [0, 1, 0, 0, 1, 1, 0, 1]
alice_bases = [0, 0, 1, 0, 1, 0, 1, 1]
bob_bases   = [0, 1, 1, 0, 0, 0, 1, 1]
alice_key, bob_key = sift_keys(alice_bits, alice_bases, bob_bases)
print(alice_key)`,
			expected: "[0, 0, 0, 1, 0, 1]\n",
		},
		{
			name: "Alice and Bob keys agree when no eavesdropping",
			code: `{{FUNC}}
alice_bits  = [0, 1, 0, 0, 1, 1, 0, 1]
alice_bases = [0, 0, 1, 0, 1, 0, 1, 1]
bob_bases   = [0, 1, 1, 0, 0, 0, 1, 1]
alice_key, bob_key = sift_keys(alice_bits, alice_bases, bob_bases)
print(alice_key == bob_key)`,
			expected: "True\n",
		},
		{
			name: "all bases matching returns all bits",
			code: `{{FUNC}}
alice_bits  = [1, 0, 1, 0]
alice_bases = [0, 0, 1, 1]
bob_bases   = [0, 0, 1, 1]
alice_key, bob_key = sift_keys(alice_bits, alice_bases, bob_bases)
print(alice_key)
print(alice_key == bob_key)`,
			expected: "[1, 0, 1, 0]\nTrue\n",
		},
	],
};
