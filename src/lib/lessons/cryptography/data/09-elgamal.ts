import type { Lesson } from "../../types";

export const elgamal: Lesson = {
	id: "elgamal",
	title: "ElGamal Encryption",
	chapterId: "asymmetric",
	content: `## ElGamal Encryption

The **ElGamal cryptosystem** (Taher ElGamal, 1985) is a public-key scheme based on the **Discrete Logarithm Problem**, like Diffie-Hellman. Unlike RSA, ElGamal encryption is **probabilistic** — the same plaintext encrypted twice produces different ciphertexts (due to the random $k$), which is a security advantage.

### Key Generation

Choose a prime $p$, generator $g$, and private key $x$. The public key is:

$$y = g^x \\bmod p$$

### Encryption

To encrypt message $m$ with public key $y$, choose a random session key $k$:

$$c_1 = g^k \\bmod p$$
$$c_2 = m \\cdot y^k \\bmod p$$

The ciphertext is the pair $(c_1, c_2)$.

### Decryption

Using private key $x$:

$$m = c_2 \\cdot c_1^{-x} \\bmod p$$

Since $c_1 = g^k$ and $y = g^x$:

$$c_1^x = g^{kx} = y^k$$

So $c_2 \\cdot c_1^{-x} = m \\cdot y^k \\cdot y^{-k} = m$. ✓

### Computing $c_1^{-x} \\bmod p$

By **Fermat's Little Theorem**, $c_1^{p-1} \\equiv 1 \\pmod{p}$, so $c_1^{-x} \\equiv c_1^{p-1-x} \\pmod{p}$.

### Example

$p=23$, $g=5$, private key $x=6$, public key $y = 5^6 \\bmod 23 = 8$.

Encrypt $m=10$ with session key $k=3$:
- $c_1 = 5^3 \\bmod 23 = 10$  ·  $c_2 = 10 \\cdot 8^3 \\bmod 23 = 10 \\cdot 6 \\bmod 23 = 14$

Decrypt $(10, 14)$: $14 \\cdot 10^{23-1-6} \\bmod 23 = 14 \\cdot 10^{16} \\bmod 23 = 10$ ✓

### Your Task

Implement:
- \`elgamal_encrypt(m, g, pub_key, k, p)\` → \`(c1, c2)\`
- \`elgamal_decrypt(c1, c2, priv_key, p)\` → \`m\``,

	starterCode: `def elgamal_encrypt(m, g, pub_key, k, p):
    # c1 = g^k mod p, c2 = m * pub_key^k mod p
    pass

def elgamal_decrypt(c1, c2, priv_key, p):
    # m = c2 * c1^(p-1-priv_key) mod p  (Fermat's little theorem for inverse)
    pass

p, g = 23, 5
priv = 6
pub = pow(g, priv, p)
m = 10
k = 3
c1, c2 = elgamal_encrypt(m, g, pub, k, p)
print(c1, c2)
print(elgamal_decrypt(c1, c2, priv, p))
`,

	solution: `def elgamal_encrypt(m, g, pub_key, k, p):
    c1 = pow(g, k, p)
    c2 = (m * pow(pub_key, k, p)) % p
    return (c1, c2)

def elgamal_decrypt(c1, c2, priv_key, p):
    return (c2 * pow(c1, p - 1 - priv_key, p)) % p

p, g = 23, 5
priv = 6
pub = pow(g, priv, p)
m = 10
k = 3
c1, c2 = elgamal_encrypt(m, g, pub, k, p)
print(c1, c2)
print(elgamal_decrypt(c1, c2, priv, p))
`,

	tests: [
		{
			name: "elgamal encrypt (p=23, g=5, priv=6, m=10, k=3) = (10, 14)",
			expected: "10 14\n10\n",
		},
		{
			name: "elgamal decrypt is inverse of encrypt",
			code: `{{FUNC}}
p, g = 23, 5
priv = 4
pub = pow(g, priv, p)
c1, c2 = elgamal_encrypt(7, g, pub, 2, p)
print(elgamal_decrypt(c1, c2, priv, p))`,
			expected: "7\n",
		},
		{
			name: "elgamal encrypt m=1 gives known c2",
			code: `{{FUNC}}
p, g = 23, 5
priv = 6
pub = pow(g, priv, p)
c1, c2 = elgamal_encrypt(1, g, pub, 3, p)
print(c1)`,
			expected: "10\n",
		},
	],
};
