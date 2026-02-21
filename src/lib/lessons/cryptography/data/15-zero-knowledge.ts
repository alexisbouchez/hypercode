import type { Lesson } from "../../types";

export const zeroKnowledge: Lesson = {
	id: "zero-knowledge",
	title: "Zero-Knowledge Proofs (Schnorr)",
	chapterId: "protocols",
	content: `## Zero-Knowledge Proofs

A **zero-knowledge proof** (ZKP) lets a **prover** convince a **verifier** that they know a secret, without revealing the secret itself. Introduced by Goldwasser, Micali, and Rackoff (1985), ZKPs are a cornerstone of modern cryptography.

### Three Properties of a ZKP

- **Completeness** — if the prover knows the secret, they can always convince the verifier
- **Soundness** — a cheating prover (who doesn't know the secret) cannot convince the verifier except with negligible probability
- **Zero-knowledge** — the verifier learns nothing about the secret beyond its existence

### Schnorr Identification Protocol

The **Schnorr protocol** (Claus Schnorr, 1989) proves knowledge of a **discrete logarithm** in zero-knowledge.

**Setup**: prime $p$, subgroup of order $q$ (where $q | p-1$), generator $g$ of order $q$.

**Public key**: $y = g^x \\bmod p$ (prover knows private key $x$).

#### Protocol (3 moves):

1. **Commit**: Prover picks random $r$, sends $t = g^r \\bmod p$
2. **Challenge**: Verifier sends random $c$
3. **Respond**: Prover sends $s = (r + c \\cdot x) \\bmod q$

#### Verification:

$$g^s \\stackrel{?}{=} t \\cdot y^c \\pmod{p}$$

This works because:
$$g^s = g^{r + cx} = g^r \\cdot (g^x)^c = t \\cdot y^c$$

### Example

$p=23$, $q=11$, $g=2$, private key $x=3$, public key $y = 2^3 \\bmod 23 = 8$.

- Commit: $r=5$, $t = 2^5 \\bmod 23 = 9$
- Challenge: $c = 4$
- Response: $s = (5 + 4 \\times 3) \\bmod 11 = 6$
- Verify: $2^6 \\bmod 23 = 64 \\bmod 23 = 18 = 9 \\cdot 8^4 \\bmod 23$ ✓

### Your Task

Implement:
- \`schnorr_commitment(g, r, p)\` → $g^r \\bmod p$
- \`schnorr_response(r, challenge, priv_key, q)\` → $(r + \\text{challenge} \\times \\text{priv\\_key}) \\bmod q$
- \`schnorr_verify(g, pub_key, commitment, challenge, response, p, q)\` → \`bool\``,

	starterCode: `def schnorr_commitment(g, r, p):
    # Compute g^r mod p
    pass

def schnorr_response(r, challenge, priv_key, q):
    # Compute (r + challenge * priv_key) % q
    pass

def schnorr_verify(g, pub_key, commitment, challenge, response, p, q):
    # Verify: g^response mod p == commitment * pub_key^challenge mod p
    pass

p, q, g = 23, 11, 2
priv = 3
pub = pow(g, priv, p)
r = 5
commitment = schnorr_commitment(g, r, p)
challenge = 4
response = schnorr_response(r, challenge, priv, q)
print(commitment)
print(response)
print(schnorr_verify(g, pub, commitment, challenge, response, p, q))
`,

	solution: `def schnorr_commitment(g, r, p):
    return pow(g, r, p)

def schnorr_response(r, challenge, priv_key, q):
    return (r + challenge * priv_key) % q

def schnorr_verify(g, pub_key, commitment, challenge, response, p, q):
    lhs = pow(g, response, p)
    rhs = (commitment * pow(pub_key, challenge, p)) % p
    return lhs == rhs

p, q, g = 23, 11, 2
priv = 3
pub = pow(g, priv, p)
r = 5
commitment = schnorr_commitment(g, r, p)
challenge = 4
response = schnorr_response(r, challenge, priv, q)
print(commitment)
print(response)
print(schnorr_verify(g, pub, commitment, challenge, response, p, q))
`,

	tests: [
		{
			name: "Schnorr protocol with p=23, q=11, g=2, priv=3",
			expected: "9\n6\nTrue\n",
		},
		{
			name: "schnorr_verify returns False with wrong response",
			code: `{{FUNC}}
p, q, g = 23, 11, 2
priv = 3
pub = pow(g, priv, p)
r = 5
commitment = schnorr_commitment(g, r, p)
challenge = 4
response = schnorr_response(r, challenge, priv, q)
print(schnorr_verify(g, pub, commitment, challenge, response + 1, p, q))`,
			expected: "False\n",
		},
		{
			name: "schnorr_commitment is just modular exponentiation",
			code: `{{FUNC}}
print(schnorr_commitment(2, 10, 23))`,
			expected: "12\n",
		},
		{
			name: "schnorr_response is linear in challenge",
			code: `{{FUNC}}
print(schnorr_response(3, 0, 7, 11))`,
			expected: "3\n",
		},
	],
};
