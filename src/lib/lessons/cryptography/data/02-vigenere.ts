import type { Lesson } from "../../types";

export const vigenereCipher: Lesson = {
	id: "vigenere",
	title: "Vigenère Cipher",
	chapterId: "classical",
	content: `## Vigenère Cipher

The **Vigenère cipher** (1553) extends the Caesar cipher by using a **keyword** instead of a single shift. Each letter of the plaintext is shifted by the corresponding letter of the key (cycling through the key).

### Encryption

Given plaintext $P$ and key $K$ (both uppercase letters), the $i$-th ciphertext letter is:

$$C_i = (P_i + K_{i \\bmod |K|}) \\bmod 26$$

where letter values are A=0, B=1, …, Z=25.

### Decryption

$$P_i = (C_i - K_{i \\bmod |K|}) \\bmod 26$$

### Example

| Index    | 0  | 1  | 2  | 3  | 4  |
|----------|----|----|----|----|----|
| Plaintext | H  | E  | L  | L  | O  |
| Key      | K  | E  | Y  | K  | E  |
| Shift    | 10 | 4  | 24 | 10 | 4  |
| Cipher   | R  | I  | J  | V  | S  |

- \`HELLO\` with key \`KEY\` → \`RIJVS\`

### Security

The Vigenère cipher defeated frequency analysis for centuries — it was called the *"le chiffre indéchiffrable"* (the indecipherable cipher). Charles Babbage and Friedrich Kasiski independently broke it in the 19th century using the **Kasiski test**, which exploits repeated key cycles.

### Your Task

Implement (uppercase alphabetic characters only; non-alpha characters are skipped):
- \`vigenere_encrypt(text, key)\` — encrypt using the Vigenère cipher
- \`vigenere_decrypt(text, key)\` — decrypt`,

	starterCode: `def vigenere_encrypt(text, key):
    # Encrypt uppercase alpha only, skip non-alpha
    # Key index advances only on alpha characters
    pass

def vigenere_decrypt(text, key):
    pass

print(vigenere_encrypt("HELLO", "KEY"))
print(vigenere_decrypt("RIJVS", "KEY"))
`,

	solution: `def vigenere_encrypt(text, key):
    result = []
    key = key.upper()
    ki = 0
    for c in text:
        if c.isalpha():
            shift = ord(key[ki % len(key)]) - ord('A')
            result.append(chr((ord(c.upper()) - ord('A') + shift) % 26 + ord('A')))
            ki += 1
        else:
            result.append(c)
    return ''.join(result)

def vigenere_decrypt(text, key):
    result = []
    key = key.upper()
    ki = 0
    for c in text:
        if c.isalpha():
            shift = ord(key[ki % len(key)]) - ord('A')
            result.append(chr((ord(c.upper()) - ord('A') - shift) % 26 + ord('A')))
            ki += 1
        else:
            result.append(c)
    return ''.join(result)

print(vigenere_encrypt("HELLO", "KEY"))
print(vigenere_decrypt("RIJVS", "KEY"))
`,

	tests: [
		{
			name: "vigenere_encrypt('HELLO', 'KEY') = 'RIJVS'",
			expected: "RIJVS\nHELLO\n",
		},
		{
			name: "vigenere_encrypt('ATTACK', 'LEMON') = 'LXFOPV'",
			code: `{{FUNC}}
print(vigenere_encrypt("ATTACK", "LEMON"))`,
			expected: "LXFOPV\n",
		},
		{
			name: "vigenere_decrypt is inverse of encrypt",
			code: `{{FUNC}}
print(vigenere_decrypt(vigenere_encrypt("PYTHON", "SECRET"), "SECRET"))`,
			expected: "PYTHON\n",
		},
		{
			name: "vigenere_encrypt with single-char key is Caesar",
			code: `{{FUNC}}
print(vigenere_encrypt("ABC", "D"))`,
			expected: "DEF\n",
		},
		{
			name: "vigenere key cycles correctly",
			code: `{{FUNC}}
print(vigenere_encrypt("AAAAAA", "AB"))`,
			expected: "ABABAB\n",
		},
	],
};
