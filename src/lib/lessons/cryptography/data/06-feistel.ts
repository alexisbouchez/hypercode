import type { Lesson } from "../../types";

export const feistelCipher: Lesson = {
	id: "feistel",
	title: "Feistel Network",
	chapterId: "symmetric",
	content: `## Feistel Network

A **Feistel network** (Horst Feistel, IBM, 1973) is a symmetric structure used in many block ciphers including DES. Its key insight: you can build an invertible cipher from a **non-invertible** round function.

### One Round

Split the block into two halves $L$ and $R$. Apply the round function $F$ with round key $K$:

$$L' = R$$
$$R' = L \\oplus F(R, K)$$

The structure is **automatically invertible** — to reverse a round:
$$R = L'$$
$$L = R' \\oplus F(L', K)$$

### Multi-round Encryption

Repeat the round with different subkeys $K_1, K_2, \\ldots, K_n$. More rounds → more diffusion and confusion.

### Our Simulation

We use 4-bit halves (from an 8-bit block: $L = \\text{byte} >> 4$, $R = \\text{byte} \\& 0xF$) with round function:

$$F(R, K) = (R + K) \\bmod 256$$

Combined in the round:
$$L' = R, \\quad R' = L \\oplus (R + K) \\bmod 256$$

### DES

DES uses a 64-bit block with 16 Feistel rounds and 56-bit keys. It was the US federal standard from 1977 to 2001, superseded by AES.

### Your Task

Implement:
- \`feistel_round(L, R, K)\` → \`(new_L, new_R)\` where \`new_L = R\`, \`new_R = L XOR (R+K)%256\`
- \`feistel_encrypt(plaintext, keys)\` — apply rounds with each key in sequence
- \`feistel_decrypt(ciphertext, keys)\` — apply rounds in reverse order using inverse round`,

	starterCode: `def feistel_round(L, R, K):
    # new_L = R, new_R = L XOR (R+K)%256
    pass

def feistel_encrypt(plaintext, keys):
    # Split: L = plaintext >> 4, R = plaintext & 0xF
    # Apply feistel_round for each key
    # Recombine: (L << 4) | R
    pass

def feistel_decrypt(ciphertext, keys):
    # Apply rounds in reverse order using inverse round
    pass

print(feistel_round(5, 3, 2))
print(feistel_encrypt(0xAB, [3, 7]))
print(feistel_decrypt(feistel_encrypt(0xAB, [3, 7]), [3, 7]))
`,

	solution: `def feistel_round(L, R, K):
    new_L = R
    new_R = L ^ ((R + K) % 256)
    return new_L, new_R

def feistel_encrypt(plaintext, keys):
    L = plaintext >> 4
    R = plaintext & 0xF
    for K in keys:
        L, R = feistel_round(L, R, K)
    return (L << 4) | R

def feistel_decrypt(ciphertext, keys):
    L = ciphertext >> 4
    R = ciphertext & 0xF
    for K in reversed(keys):
        L_old = R ^ ((L + K) % 256)
        R_old = L
        L, R = L_old, R_old
    return (L << 4) | R

print(feistel_round(5, 3, 2))
print(feistel_encrypt(0xAB, [3, 7]))
print(feistel_decrypt(feistel_encrypt(0xAB, [3, 7]), [3, 7]))
`,

	tests: [
		{
			name: "feistel_round(5, 3, 2) = (3, 0)",
			expected: "(3, 0)\n64\n171\n",
		},
		{
			name: "feistel_decrypt inverts feistel_encrypt for keys [1,2]",
			code: `{{FUNC}}
ct = feistel_encrypt(42, [1, 2])
print(feistel_decrypt(ct, [1, 2]))`,
			expected: "42\n",
		},
		{
			name: "feistel_round swaps halves with XOR diffusion",
			code: `{{FUNC}}
L, R = feistel_round(0, 0, 5)
print(L, R)`,
			expected: "0 5\n",
		},
		{
			name: "feistel_encrypt then decrypt with single key",
			code: `{{FUNC}}
print(feistel_decrypt(feistel_encrypt(0x12, [3]), [3]))`,
			expected: "18\n",
		},
	],
};
