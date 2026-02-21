import type { Lesson } from "../../types";

export const hmacSimple: Lesson = {
	id: "hmac-simple",
	title: "HMAC — Keyed Hash",
	chapterId: "hash",
	content: `## HMAC — Hash-based Message Authentication Code

A bare hash $H(m)$ does not authenticate who sent $m$ — anyone can compute it. A **MAC** (Message Authentication Code) requires knowing a secret key $k$ to produce the authentication tag.

### Naive Approach and Its Flaw

The simplest idea: $H(k \\| m)$ (concatenate key and message). This is vulnerable to a **length-extension attack** for hash functions like SHA-1 and SHA-2: given $H(k \\| m)$, an attacker can compute $H(k \\| m \\| \\text{extra})$ without knowing $k$.

### HMAC Construction

The **HMAC** standard (RFC 2104) avoids length extension by applying the key twice:

$$\\text{HMAC}(k, m) = H\\bigl((k \\oplus \\text{opad}) \\| H((k \\oplus \\text{ipad}) \\| m)\\bigr)$$

where \`ipad = 0x36 repeated\` and \`opad = 0x5C repeated\`.

### Our Simplified HMAC

We implement a simplified version using DJB2:

$$\\text{hmac\\_simple}(m, k) = \\text{djb2}(k \\| m \\| k) \\bmod 2^{32}$$

Wrapping the message with the key on both sides (like HMAC's double-key structure) prevents simple concatenation attacks.

### Properties of MACs

- **Unforgeability** — without the key, an attacker cannot produce a valid MAC
- **Verification** — recompute MAC and compare; use constant-time comparison in production
- **Different keys → different MACs** for the same message

### Your Task

Implement:
- \`djb2_hash(s)\` — DJB2 hash (hash=5381, hash = hash*33 XOR ord(c), return hash & 0xFFFFFFFF)
- \`hmac_simple(message, key)\` — returns \`djb2_hash(key + message + key) & 0xFFFFFFFF\``,

	starterCode: `def djb2_hash(s):
    hash_val = 5381
    for c in s:
        hash_val = (hash_val * 33) ^ ord(c)
    return hash_val & 0xFFFFFFFF

def hmac_simple(message, key):
    # Compute djb2_hash(key + message + key) & 0xFFFFFFFF
    pass

print(hmac_simple("hello", "secret"))
print(hmac_simple("world", "secret"))
`,

	solution: `def djb2_hash(s):
    hash_val = 5381
    for c in s:
        hash_val = (hash_val * 33) ^ ord(c)
    return hash_val & 0xFFFFFFFF

def hmac_simple(message, key):
    return djb2_hash(key + message + key) & 0xFFFFFFFF

print(hmac_simple("hello", "secret"))
print(hmac_simple("world", "secret"))
`,

	tests: [
		{
			name: "hmac_simple('hello', 'secret') = 3732535079",
			expected: "3732535079\n3329438119\n",
		},
		{
			name: "different keys give different MACs for same message",
			code: `{{FUNC}}
print(hmac_simple("hello", "key1") == hmac_simple("hello", "key2"))`,
			expected: "False\n",
		},
		{
			name: "same key and message always gives same MAC",
			code: `{{FUNC}}
print(hmac_simple("hello", "key") == hmac_simple("hello", "key"))`,
			expected: "True\n",
		},
		{
			name: "hmac changes when message changes",
			code: `{{FUNC}}
print(hmac_simple("hello", "k") == hmac_simple("Hello", "k"))`,
			expected: "False\n",
		},
	],
};
