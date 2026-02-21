import type { Lesson } from "../../types";

export const affineCipher: Lesson = {
	id: "affine-cipher",
	title: "Affine Cipher",
	chapterId: "symmetric",
	content: `## Affine Cipher

The **affine cipher** generalizes the Caesar cipher with a linear transformation over $\\mathbb{Z}_{26}$.

### Encryption

$$E(x) = (a \\cdot x + b) \\bmod 26$$

where $x$ is the letter value (A=0, …, Z=25), $a$ and $b$ are integer keys. $a$ must satisfy $\\gcd(a, 26) = 1$ so that decryption is possible.

Valid values of $a$: 1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25 (12 choices).

### Decryption

$$D(y) = a^{-1} \\cdot (y - b) \\bmod 26$$

where $a^{-1}$ is the **modular inverse** of $a$ modulo 26.

### Modular Inverse via Extended GCD

The **Extended Euclidean Algorithm** finds integers $x, y$ such that $ax + 26y = \\gcd(a, 26)$. When $\\gcd(a, 26) = 1$, we have $ax \\equiv 1 \\pmod{26}$, so $x$ is the modular inverse.

\`\`\`python
def extended_gcd(a, b):
    if b == 0: return a, 1, 0
    g, x, y = extended_gcd(b, a % b)
    return g, y, x - (a // b) * y

def mod_inverse(a, m):
    g, x, _ = extended_gcd(a, m)
    return x % m   # ensure positive

mod_inverse(5, 26)   # 21, since 5*21 = 105 = 4*26+1
\`\`\`

### Example ($a=5$, $b=8$)

| Plaintext | H(7) | E(4) | L(11) | L(11) | O(14) |
|-----------|------|------|-------|-------|-------|
| $5x+8$ mod 26 | 17 | 2 | 11 | 11 | 0 |
| Ciphertext | R | C | L | L | A |

### Your Task

Implement:
- \`mod_inverse(a, m)\` using extended GCD
- \`affine_encrypt(text, a, b)\` — apply affine transformation, preserve case, skip non-alpha
- \`affine_decrypt(text, a, b)\` — apply inverse transformation`,

	starterCode: `def mod_inverse(a, m):
    # Use extended GCD to find a^(-1) mod m
    pass

def affine_encrypt(text, a, b):
    # Encrypt: E(x) = (a*x + b) mod 26
    pass

def affine_decrypt(text, a, b):
    # Decrypt: D(y) = a_inv * (y - b) mod 26
    pass

print(affine_encrypt("HELLO", 5, 8))
print(affine_decrypt("RCLLA", 5, 8))
`,

	solution: `def mod_inverse(a, m):
    def extended_gcd(a, b):
        if b == 0:
            return a, 1, 0
        g, x, y = extended_gcd(b, a % b)
        return g, y, x - (a // b) * y
    g, x, _ = extended_gcd(a, m)
    return x % m

def affine_encrypt(text, a, b):
    result = []
    for c in text:
        if c.isalpha():
            base = ord('A') if c.isupper() else ord('a')
            result.append(chr((a * (ord(c) - base) + b) % 26 + base))
        else:
            result.append(c)
    return ''.join(result)

def affine_decrypt(text, a, b):
    a_inv = mod_inverse(a, 26)
    result = []
    for c in text:
        if c.isalpha():
            base = ord('A') if c.isupper() else ord('a')
            result.append(chr((a_inv * (ord(c) - base - b)) % 26 + base))
        else:
            result.append(c)
    return ''.join(result)

print(affine_encrypt("HELLO", 5, 8))
print(affine_decrypt("RCLLA", 5, 8))
`,

	tests: [
		{
			name: "affine_encrypt('HELLO', 5, 8) = 'RCLLA'",
			expected: "RCLLA\nHELLO\n",
		},
		{
			name: "mod_inverse(5, 26) = 21",
			code: `{{FUNC}}
print(mod_inverse(5, 26))`,
			expected: "21\n",
		},
		{
			name: "affine_decrypt is inverse of affine_encrypt",
			code: `{{FUNC}}
print(affine_decrypt(affine_encrypt("PYTHON", 7, 3), 7, 3))`,
			expected: "PYTHON\n",
		},
		{
			name: "affine_encrypt with a=1 is Caesar",
			code: `{{FUNC}}
print(affine_encrypt("HELLO", 1, 3))`,
			expected: "KHOOR\n",
		},
		{
			name: "mod_inverse(3, 10) = 7",
			code: `{{FUNC}}
print(mod_inverse(3, 10))`,
			expected: "7\n",
		},
	],
};
