import type { Lesson } from "../../types";

export const caesarCipher: Lesson = {
	id: "caesar-cipher",
	title: "Caesar Cipher",
	chapterId: "classical",
	content: `## Caesar Cipher

The **Caesar cipher** is one of the oldest and simplest encryption techniques, named after Julius Caesar who reportedly used it to protect his military communications. It is a **substitution cipher** where each letter in the plaintext is shifted a fixed number of positions down the alphabet.

### Encryption

For a shift of $k$, each letter at position $p$ (0-indexed, A=0, B=1, …, Z=25) becomes:

$$c = (p + k) \\bmod 26$$

### Decryption

Decryption simply shifts in the opposite direction:

$$p = (c - k) \\bmod 26$$

### Example (shift = 3)

| Plaintext  | H  | E  | L  | L  | O  |
|------------|----|----|----|----|-----|
| Shift +3   | K  | H  | O  | O  | R  |

- \`HELLO\` → \`KHOOR\`

### ROT13

A special case with $k = 13$: since $13 + 13 = 26$, encryption and decryption are the same operation. ROT13 is its own inverse.

### Security

The Caesar cipher has only 26 possible keys (0–25) and is trivially broken by **brute force** or **frequency analysis** — the letter distribution of English is preserved.

### Your Task

Implement:
- \`caesar_encrypt(text, shift)\` — shift each alpha character by \`shift\` positions, preserving case and leaving non-alpha characters unchanged
- \`caesar_decrypt(text, shift)\` — reverse the shift`,

	starterCode: `def caesar_encrypt(text, shift):
    # Shift each alphabetic character by shift positions
    # Preserve case, leave non-alpha characters unchanged
    pass

def caesar_decrypt(text, shift):
    # Reverse the shift
    pass

print(caesar_encrypt("HELLO", 3))
print(caesar_decrypt("KHOOR", 3))
`,

	solution: `def caesar_encrypt(text, shift):
    result = []
    for c in text:
        if c.isalpha():
            base = ord('A') if c.isupper() else ord('a')
            result.append(chr((ord(c) - base + shift) % 26 + base))
        else:
            result.append(c)
    return ''.join(result)

def caesar_decrypt(text, shift):
    return caesar_encrypt(text, -shift)

print(caesar_encrypt("HELLO", 3))
print(caesar_decrypt("KHOOR", 3))
`,

	tests: [
		{
			name: "caesar_encrypt('HELLO', 3) = 'KHOOR'",
			expected: "KHOOR\nHELLO\n",
		},
		{
			name: "caesar_encrypt with ROT13",
			code: `{{FUNC}}
print(caesar_encrypt("Hello, World!", 13))`,
			expected: "Uryyb, Jbeyq!\n",
		},
		{
			name: "caesar_decrypt is inverse of encrypt",
			code: `{{FUNC}}
print(caesar_decrypt(caesar_encrypt("Python", 7), 7))`,
			expected: "Python\n",
		},
		{
			name: "caesar_encrypt shift 0 is identity",
			code: `{{FUNC}}
print(caesar_encrypt("ABC", 0))`,
			expected: "ABC\n",
		},
		{
			name: "caesar_encrypt wraps around Z",
			code: `{{FUNC}}
print(caesar_encrypt("XYZ", 3))`,
			expected: "ABC\n",
		},
	],
};
