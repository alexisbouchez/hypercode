import type { Lesson } from "../../types";

export const xorCipher: Lesson = {
	id: "xor-cipher",
	title: "XOR Cipher",
	chapterId: "classical",
	content: `## XOR Cipher

The **XOR cipher** uses the exclusive-or operation to combine plaintext bytes with a key byte. It is a building block for many modern ciphers.

### The XOR Operation

XOR (⊕) has a remarkable property: it is its own inverse.

$$a \\oplus b \\oplus b = a$$

This means encryption and decryption are **identical operations**: XOR the data with the key.

### Byte-level Encryption

Given a list of plaintext bytes $[p_0, p_1, \\ldots]$ and a single-byte key $k$:

$$c_i = p_i \\oplus k$$

Decryption:

$$p_i = c_i \\oplus k$$

### Example

Encrypting \`"Hi"\` (ASCII 72, 105) with key 42:

| Byte | Plaintext | Key | Ciphertext |
|------|-----------|-----|-----------|
| 0    | 72 (H)    | 42  | 98        |
| 1    | 105 (i)   | 42  | 67        |

### Security

Single-byte XOR is trivially broken by **frequency analysis** — just try all 256 key values. Multi-byte XOR (Vigenère over bytes) is similarly weak. However, XOR with a **truly random key as long as the message** is the **One-Time Pad** — provably unbreakable (Shannon, 1949).

### Your Task

Implement:
- \`xor_encrypt(data, key)\` — XOR each byte in \`data\` (list of ints) with \`key\`, return new list
- \`xor_decrypt(data, key)\` — identical operation (XOR is its own inverse)`,

	starterCode: `def xor_encrypt(data, key):
    # XOR each integer in data with key
    pass

def xor_decrypt(data, key):
    # XOR is its own inverse — same as encrypt
    pass

print(xor_encrypt([72, 101, 108, 108, 111], 42))
print(xor_decrypt([98, 67, 70, 70, 69], 42))
`,

	solution: `def xor_encrypt(data, key):
    return [b ^ key for b in data]

def xor_decrypt(data, key):
    return [b ^ key for b in data]

print(xor_encrypt([72, 101, 108, 108, 111], 42))
print(xor_decrypt([98, 67, 70, 70, 69], 42))
`,

	tests: [
		{
			name: "xor_encrypt([72,101,108,108,111], 42) = [98,79,70,70,69]",
			expected: "[98, 79, 70, 70, 69]\n[72, 105, 108, 108, 111]\n",
		},
		{
			name: "xor_decrypt is inverse of xor_encrypt",
			code: `{{FUNC}}
print(xor_decrypt(xor_encrypt([1, 2, 3, 4, 5], 255), 255))`,
			expected: "[1, 2, 3, 4, 5]\n",
		},
		{
			name: "xor with key 0 is identity",
			code: `{{FUNC}}
print(xor_encrypt([10, 20, 30], 0))`,
			expected: "[10, 20, 30]\n",
		},
		{
			name: "xor with 0xFF flips all bits",
			code: `{{FUNC}}
print(xor_encrypt([0xFF, 0x00, 0xAA], 0xFF))`,
			expected: "[0, 255, 85]\n",
		},
	],
};
