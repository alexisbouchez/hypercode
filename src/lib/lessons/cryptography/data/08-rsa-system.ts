import type { Lesson } from "../../types";

export const rsaSystem: Lesson = {
	id: "rsa-system",
	title: "RSA Cryptosystem",
	chapterId: "asymmetric",
	content: `## RSA Cryptosystem

**RSA** (Rivest–Shamir–Adleman, 1977) is the most widely deployed public-key cryptosystem. Its security rests on the **integer factorization problem** — factoring large numbers is believed to be computationally infeasible.

### Key Generation

1. Choose two distinct primes $p$ and $q$
2. Compute $n = p \\cdot q$ (the **modulus**)
3. Compute $\\varphi(n) = (p-1)(q-1)$ (Euler's totient)
4. Choose $e$ coprime to $\\varphi(n)$ (the **public exponent**; common choice: $e = 65537$)
5. Compute $d \\equiv e^{-1} \\pmod{\\varphi(n)}$ (the **private exponent**)

Public key: $(e, n)$ · Private key: $(d, n)$

### Encryption and Decryption

$$\\text{encrypt}(m) = m^e \\bmod n$$
$$\\text{decrypt}(c) = c^d \\bmod n$$

### Why It Works

By Euler's theorem: $m^{\\varphi(n)} \\equiv 1 \\pmod{n}$, so:

$$c^d = (m^e)^d = m^{ed} = m^{1 + k\\varphi(n)} = m \\cdot (m^{\\varphi(n)})^k \\equiv m \\pmod{n}$$

### Classic Example

$p = 61$, $q = 53$, $e = 17$:
- $n = 3233$
- $\\varphi(n) = 3120$
- $d = 17^{-1} \\bmod 3120 = 2753$

Encrypt 42: $42^{17} \\bmod 3233 = 2557$
Decrypt 2557: $2557^{2753} \\bmod 3233 = 42$

### Your Task

Implement:
- \`rsa_keypair(p, q, e)\` → \`(n, e, d)\`
- \`rsa_encrypt(m, e, n)\` — returns $m^e \\bmod n$
- \`rsa_decrypt(c, d, n)\` — returns $c^d \\bmod n$

Use Python's built-in \`pow(base, exp, mod)\` for efficient modular exponentiation.`,

	starterCode: `def rsa_keypair(p, q, e):
    # Compute n = p*q, phi = (p-1)*(q-1), d = e^-1 mod phi
    # Return (n, e, d)
    pass

def rsa_encrypt(m, e, n):
    # Return m^e mod n
    pass

def rsa_decrypt(c, d, n):
    # Return c^d mod n
    pass

n, e, d = rsa_keypair(61, 53, 17)
print(n, e, d)
c = rsa_encrypt(42, e, n)
print(c)
print(rsa_decrypt(c, d, n))
`,

	solution: `def rsa_keypair(p, q, e):
    n = p * q
    phi = (p - 1) * (q - 1)
    def extended_gcd(a, b):
        if b == 0:
            return a, 1, 0
        g, x, y = extended_gcd(b, a % b)
        return g, y, x - (a // b) * y
    _, x, _ = extended_gcd(e, phi)
    d = x % phi
    return (n, e, d)

def rsa_encrypt(m, e, n):
    return pow(m, e, n)

def rsa_decrypt(c, d, n):
    return pow(c, d, n)

n, e, d = rsa_keypair(61, 53, 17)
print(n, e, d)
c = rsa_encrypt(42, e, n)
print(c)
print(rsa_decrypt(c, d, n))
`,

	tests: [
		{
			name: "rsa_keypair(61, 53, 17) = (3233, 17, 2753)",
			expected: "3233 17 2753\n2557\n42\n",
		},
		{
			name: "rsa encrypt then decrypt returns original",
			code: `{{FUNC}}
n, e, d = rsa_keypair(61, 53, 17)
print(rsa_decrypt(rsa_encrypt(100, e, n), d, n))`,
			expected: "100\n",
		},
		{
			name: "rsa_encrypt(1, e, n) = 1",
			code: `{{FUNC}}
n, e, d = rsa_keypair(61, 53, 17)
print(rsa_encrypt(1, e, n))`,
			expected: "1\n",
		},
		{
			name: "rsa_encrypt(0, e, n) = 0",
			code: `{{FUNC}}
n, e, d = rsa_keypair(61, 53, 17)
print(rsa_encrypt(0, e, n))`,
			expected: "0\n",
		},
		{
			name: "rsa works with different primes",
			code: `{{FUNC}}
n, e, d = rsa_keypair(5, 11, 3)
print(rsa_decrypt(rsa_encrypt(7, e, n), d, n))`,
			expected: "7\n",
		},
	],
};
