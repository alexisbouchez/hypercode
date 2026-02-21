import type { Lesson } from "../../types";

export const commitmentScheme: Lesson = {
	id: "commitment-scheme",
	title: "Commitment Scheme",
	chapterId: "protocols",
	content: `## Cryptographic Commitment Scheme

A **commitment scheme** lets one party (the committer) lock in a value without revealing it, then later prove they committed to that specific value. Think of sealing a prediction in an envelope — you can't change it after the fact, but the recipient can't see it yet.

### Properties

- **Hiding**: the commitment $C$ reveals nothing about the value $v$
- **Binding**: once committed, the committer cannot change $v$ to a different value that produces the same $C$

### Construction with a Nonce

To commit to value $v$, choose a random **nonce** (number used once) $r$:

$$C = H(v, r)$$

To **reveal**: publish $(v, r)$. Anyone can verify by recomputing $H(v, r) = C$.

The nonce prevents dictionary attacks: without $r$, guessing $v$ from $C$ is infeasible.

### Applications

- **Mental poker** — commit to cards before any player reveals theirs
- **Coin flipping over the wire** — both parties commit to random bits, then reveal simultaneously
- **Zero-knowledge proofs** — commitments are a core building block
- **Auction systems** — sealed bids that are opened simultaneously

### Our Simulation

We simulate commitment with a simple hash:

$$\\text{commit}(v, r) = (v \\times 1000003 \\oplus r) \\bmod 2^{32}$$

(1000003 is a prime that gives good mixing.)

### Your Task

Implement:
- \`commit(value, nonce)\` → \`(value * 1000003 ^ nonce) & 0xFFFFFFFF\`
- \`reveal(commitment, value, nonce)\` → \`bool\`: check if recomputing gives the same commitment`,

	starterCode: `def commit(value, nonce):
    # Return (value * 1000003 ^ nonce) & 0xFFFFFFFF
    pass

def reveal(commitment, value, nonce):
    # Return True if commit(value, nonce) == commitment
    pass

c = commit(42, 12345)
print(c)
print(reveal(c, 42, 12345))
print(reveal(c, 43, 12345))
`,

	solution: `def commit(value, nonce):
    return (value * 1000003 ^ nonce) & 0xFFFFFFFF

def reveal(commitment, value, nonce):
    return commit(value, nonce) == commitment

c = commit(42, 12345)
print(c)
print(reveal(c, 42, 12345))
print(reveal(c, 43, 12345))
`,

	tests: [
		{
			name: "commit(42, 12345) and reveal",
			expected: "42004167\nTrue\nFalse\n",
		},
		{
			name: "reveal with wrong nonce returns False",
			code: `{{FUNC}}
c = commit(42, 12345)
print(reveal(c, 42, 12346))`,
			expected: "False\n",
		},
		{
			name: "commit(0, 0) = 0",
			code: `{{FUNC}}
print(commit(0, 0))`,
			expected: "0\n",
		},
		{
			name: "commit is deterministic",
			code: `{{FUNC}}
print(commit(100, 999) == commit(100, 999))`,
			expected: "True\n",
		},
		{
			name: "different values produce different commitments",
			code: `{{FUNC}}
print(commit(1, 0) == commit(2, 0))`,
			expected: "False\n",
		},
	],
};
