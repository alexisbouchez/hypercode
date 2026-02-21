import type { Lesson } from "../../types";

export const diffieHellman: Lesson = {
	id: "diffie-hellman",
	title: "Diffie-Hellman Key Exchange",
	chapterId: "asymmetric",
	content: `## Diffie-Hellman Key Exchange

Published by Whitfield Diffie and Martin Hellman in 1976, the **Diffie-Hellman key exchange** was the first practical public-key protocol. It allows two parties to establish a **shared secret over an insecure channel** without ever sending the secret itself.

### The Protocol

Both parties agree on public parameters: a prime $p$ and a generator $g$ (a primitive root modulo $p$).

1. **Alice** picks a private key $a$, computes public key $A = g^a \\bmod p$
2. **Bob** picks a private key $b$, computes public key $B = g^b \\bmod p$
3. Alice and Bob exchange $A$ and $B$ publicly
4. **Alice** computes $s = B^a \\bmod p = g^{ab} \\bmod p$
5. **Bob** computes $s = A^b \\bmod p = g^{ab} \\bmod p$

Both arrive at the same shared secret $s = g^{ab} \\bmod p$.

### Why It's Secure

An eavesdropper sees $g$, $p$, $A = g^a \\bmod p$, and $B = g^b \\bmod p$, but cannot compute $g^{ab} \\bmod p$ without solving the **Discrete Logarithm Problem** — finding $a$ from $g^a \\bmod p$. This is believed to be computationally hard for large $p$.

### Classic Example

$g = 2$, $p = 23$ (a prime), $a = 6$, $b = 15$:

| Party | Private | Public |
|-------|---------|--------|
| Alice | $a = 6$ | $A = 2^6 \\bmod 23 = 18$ |
| Bob   | $b = 15$ | $B = 2^{15} \\bmod 23 = 16$ |

Shared secret: $B^a \\bmod 23 = 16^6 \\bmod 23 = 4 = A^b \\bmod 23$.

### Your Task

Implement:
- \`dh_public_key(g, private_key, p)\` — compute $g^{\\text{private\\_key}} \\bmod p$
- \`dh_shared_secret(other_public, private_key, p)\` — compute $\\text{other\\_public}^{\\text{private\\_key}} \\bmod p$`,

	starterCode: `def dh_public_key(g, private_key, p):
    # Compute g^private_key mod p
    pass

def dh_shared_secret(other_public, private_key, p):
    # Compute other_public^private_key mod p
    pass

g, p = 2, 23
a, b = 6, 15
A = dh_public_key(g, a, p)
B = dh_public_key(g, b, p)
print(A)
print(B)
print(dh_shared_secret(B, a, p))
print(dh_shared_secret(A, b, p))
`,

	solution: `def dh_public_key(g, private_key, p):
    return pow(g, private_key, p)

def dh_shared_secret(other_public, private_key, p):
    return pow(other_public, private_key, p)

g, p = 2, 23
a, b = 6, 15
A = dh_public_key(g, a, p)
B = dh_public_key(g, b, p)
print(A)
print(B)
print(dh_shared_secret(B, a, p))
print(dh_shared_secret(A, b, p))
`,

	tests: [
		{
			name: "DH public keys and shared secret with g=2, p=23, a=6, b=15",
			expected: "18\n16\n4\n4\n",
		},
		{
			name: "shared secrets are equal for both parties",
			code: `{{FUNC}}
g, p, a, b = 5, 23, 4, 9
A = dh_public_key(g, a, p)
B = dh_public_key(g, b, p)
print(dh_shared_secret(B, a, p) == dh_shared_secret(A, b, p))`,
			expected: "True\n",
		},
		{
			name: "dh_public_key with private_key=1 returns g",
			code: `{{FUNC}}
print(dh_public_key(7, 1, 23))`,
			expected: "7\n",
		},
		{
			name: "dh_public_key with private_key=0 returns 1",
			code: `{{FUNC}}
print(dh_public_key(5, 0, 23))`,
			expected: "1\n",
		},
	],
};
