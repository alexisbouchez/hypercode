import type { Lesson } from "../../types";

export const cryptoApplications: Lesson = {
	id: "crypto-applications",
	title: "Applications to Cryptography",
	chapterId: "fields",
	content: `## Applications to Cryptography

Abstract algebra underpins modern cryptography. Let's implement two foundational protocols using finite field arithmetic.

### Diffie-Hellman Key Exchange

Two parties (Alice and Bob) agree on a public prime $p$ and primitive root $g$. Then:

1. Alice picks secret $a$, computes $A = g^a \\bmod p$, sends $A$ to Bob
2. Bob picks secret $b$, computes $B = g^b \\bmod p$, sends $B$ to Alice
3. Both compute the shared secret: $s = B^a \\bmod p = A^b \\bmod p = g^{ab} \\bmod p$

An eavesdropper sees $g, p, A, B$ but computing $a$ from $A = g^a \\bmod p$ requires solving the discrete logarithm — believed to be hard for large primes.

### ElGamal Encryption

Built on top of Diffie-Hellman:

**Key Generation**: Choose prime $p$, generator $g$, secret key $x$. Public key is $h = g^x \\bmod p$.

**Encrypt** message $m$ (where $1 \\le m < p$):
1. Pick random $k$
2. $c_1 = g^k \\bmod p$
3. $c_2 = (m \\cdot h^k) \\bmod p$
4. Ciphertext is $(c_1, c_2)$

**Decrypt**:
$$m = c_2 \\cdot (c_1^x)^{-1} \\bmod p = c_2 \\cdot c_1^{p-1-x} \\bmod p$$

### Modular Exponentiation

Both protocols rely on fast modular exponentiation:

\`\`\`python
def mod_pow(base, exp, mod):
    result = 1
    base = base % mod
    while exp > 0:
        if exp & 1:
            result = (result * base) % mod
        exp >>= 1
        base = (base * base) % mod
    return result
\`\`\`

### Your Task

Implement \`diffie_hellman(g, p, a, b)\` returning the shared secret, and \`elgamal_encrypt(p, g, h, m, k)\` / \`elgamal_decrypt(p, x, c1, c2)\` for ElGamal encryption.`,

	starterCode: `def mod_pow(base, exp, mod):
    result = 1
    base = base % mod
    while exp > 0:
        if exp & 1:
            result = (result * base) % mod
        exp >>= 1
        base = (base * base) % mod
    return result

def diffie_hellman(g, p, a, b):
    # Return (A, B, shared_secret)
    pass

def elgamal_encrypt(p, g, h, m, k):
    # Return (c1, c2)
    pass

def elgamal_decrypt(p, x, c1, c2):
    # Return decrypted message m
    pass

# DH with p=23, g=5, a=6, b=15
A, B, secret = diffie_hellman(5, 23, 6, 15)
print(A, B, secret)

# ElGamal: p=23, g=5, x=7, h=5^7 mod 23=17
h = mod_pow(5, 7, 23)
c1, c2 = elgamal_encrypt(23, 5, h, 10, 3)
print(c1, c2)
m = elgamal_decrypt(23, 7, c1, c2)
print(m)
`,

	solution: `def mod_pow(base, exp, mod):
    result = 1
    base = base % mod
    while exp > 0:
        if exp & 1:
            result = (result * base) % mod
        exp >>= 1
        base = (base * base) % mod
    return result

def diffie_hellman(g, p, a, b):
    A = mod_pow(g, a, p)
    B = mod_pow(g, b, p)
    secret = mod_pow(B, a, p)
    return (A, B, secret)

def elgamal_encrypt(p, g, h, m, k):
    c1 = mod_pow(g, k, p)
    c2 = (m * mod_pow(h, k, p)) % p
    return (c1, c2)

def elgamal_decrypt(p, x, c1, c2):
    s_inv = mod_pow(c1, p - 1 - x, p)
    return (c2 * s_inv) % p

# DH with p=23, g=5, a=6, b=15
A, B, secret = diffie_hellman(5, 23, 6, 15)
print(A, B, secret)

# ElGamal: p=23, g=5, x=7, h=5^7 mod 23=17
h = mod_pow(5, 7, 23)
c1, c2 = elgamal_encrypt(23, 5, h, 10, 3)
print(c1, c2)
m = elgamal_decrypt(23, 7, c1, c2)
print(m)
`,

	tests: [
		{
			name: "Diffie-Hellman and ElGamal",
			expected: "8 19 2\n10 2\n10\n",
		},
		{
			name: "DH shared secret is symmetric",
			code: `{{FUNC}}
A, B, s1 = diffie_hellman(5, 23, 6, 15)
s2 = mod_pow(A, 15, 23)
print(s1 == s2)`,
			expected: "True\n",
		},
		{
			name: "ElGamal decrypt recovers original message",
			code: `{{FUNC}}
p, g, x = 47, 5, 11
h = mod_pow(g, x, p)
for m in [1, 15, 46]:
    c1, c2 = elgamal_encrypt(p, g, h, m, 7)
    print(elgamal_decrypt(p, x, c1, c2))`,
			expected: "1\n15\n46\n",
		},
	],
};
