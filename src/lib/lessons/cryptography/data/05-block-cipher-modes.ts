import type { Lesson } from "../../types";

export const blockCipherModes: Lesson = {
	id: "block-cipher-modes",
	title: "Block Cipher Modes",
	chapterId: "symmetric",
	content: `## Block Cipher Modes of Operation

A **block cipher** encrypts fixed-size blocks of plaintext. The same key encrypts each block, but **modes of operation** define how blocks are chained together. The choice of mode dramatically affects security.

### ECB — Electronic Codebook

The simplest mode: each block is encrypted independently.

$$C_i = E_k(P_i)$$

**Fatal flaw**: identical plaintext blocks produce identical ciphertext blocks. The famous "ECB penguin" — encrypting a bitmap in ECB mode reveals the image structure.

### CBC — Cipher Block Chaining

Each block is XORed with the previous ciphertext block before encryption:

$$C_i = E_k(P_i \\oplus C_{i-1})$$

The first block uses an **Initialization Vector (IV)**:

$$C_0 = E_k(P_0 \\oplus IV)$$

**Decryption**:

$$P_i = D_k(C_i) \\oplus C_{i-1}$$

CBC eliminates the ECB pattern flaw. Each ciphertext block depends on all previous plaintext blocks.

### Our Simulation

We simulate the block cipher $E_k$ with simple XOR: $E_k(x) = x \\oplus k$.

- **ECB**: $C_i = P_i \\oplus k$
- **CBC encrypt**: $C_i = (P_i \\oplus C_{i-1}) \\oplus k$, starting with $C_{-1} = IV$
- **CBC decrypt**: $P_i = (C_i \\oplus k) \\oplus C_{i-1}$

### Your Task

Implement:
- \`ecb_encrypt(blocks, key)\` — XOR each block with key
- \`cbc_encrypt(blocks, key, iv)\` — CBC encryption
- \`cbc_decrypt(blocks, key, iv)\` — CBC decryption`,

	starterCode: `def ecb_encrypt(blocks, key):
    # XOR each block with key
    pass

def cbc_encrypt(blocks, key, iv):
    # CBC: XOR each block with previous ciphertext (start with iv), then XOR with key
    pass

def cbc_decrypt(blocks, key, iv):
    # CBC decrypt: XOR each block with key, then XOR with previous ciphertext
    pass

print(ecb_encrypt([10, 20, 30], 5))
print(cbc_encrypt([10, 20, 30], 5, 0))
print(cbc_decrypt(cbc_encrypt([10, 20, 30], 5, 0), 5, 0))
`,

	solution: `def ecb_encrypt(blocks, key):
    return [b ^ key for b in blocks]

def cbc_encrypt(blocks, key, iv):
    result = []
    prev = iv
    for b in blocks:
        enc = (b ^ prev) ^ key
        result.append(enc)
        prev = enc
    return result

def cbc_decrypt(blocks, key, iv):
    result = []
    prev = iv
    for b in blocks:
        dec = (b ^ key) ^ prev
        result.append(dec)
        prev = b
    return result

print(ecb_encrypt([10, 20, 30], 5))
print(cbc_encrypt([10, 20, 30], 5, 0))
print(cbc_decrypt(cbc_encrypt([10, 20, 30], 5, 0), 5, 0))
`,

	tests: [
		{
			name: "ecb_encrypt([10,20,30], 5) = [15,17,27]",
			expected: "[15, 17, 27]\n[15, 30, 5]\n[10, 20, 30]\n",
		},
		{
			name: "cbc_decrypt reverses cbc_encrypt for arbitrary input",
			code: `{{FUNC}}
plain = [100, 200, 150]
enc = cbc_encrypt(plain, 42, 7)
print(cbc_decrypt(enc, 42, 7))`,
			expected: "[100, 200, 150]\n",
		},
		{
			name: "ecb identical blocks produce identical ciphertext",
			code: `{{FUNC}}
enc = ecb_encrypt([5, 5, 5], 3)
print(enc[0] == enc[1] == enc[2])`,
			expected: "True\n",
		},
		{
			name: "cbc identical blocks produce different ciphertext",
			code: `{{FUNC}}
enc = cbc_encrypt([5, 5, 5], 3, 0)
print(enc[0] == enc[1])`,
			expected: "False\n",
		},
	],
};
