import type { Lesson } from "../../types";

export const simpleHash: Lesson = {
	id: "simple-hash",
	title: "Hash Functions",
	chapterId: "hash",
	content: `## Hash Functions

A **cryptographic hash function** maps an arbitrary-length input to a fixed-length output (the **digest** or **hash**). Good hash functions satisfy:

- **Deterministic** — same input always produces same output
- **Pre-image resistance** — given $h$, hard to find $m$ with $H(m) = h$
- **Second pre-image resistance** — given $m_1$, hard to find $m_2 \\neq m_1$ with same hash
- **Collision resistance** — hard to find any pair $(m_1, m_2)$ with the same hash
- **Avalanche effect** — small input change drastically changes output

### DJB2 Hash

Invented by Daniel J. Bernstein, DJB2 is a simple non-cryptographic hash widely used in hash tables. Starting with the "magic" seed 5381:

$$h_0 = 5381$$
$$h_{i+1} = (h_i \\times 33) \\oplus \\text{ord}(c_i)$$
$$\\text{result} = h_n \\bmod 2^{32}$$

The multiplication by 33 ($= 2^5 + 1$) combined with XOR gives good avalanche properties.

### Polynomial Hash

A polynomial rolling hash uses a base $b$ and prime modulus $m$:

$$H(s) = \\sum_{i=0}^{n-1} \\text{ord}(s_i) \\cdot b^i \\bmod m$$

This is the foundation of **Rabin-Karp** string matching — a rolling window keeps the polynomial hash updated in $O(1)$ per step.

### Your Task

Implement:
- \`djb2_hash(s)\` → 32-bit unsigned int (use \`& 0xFFFFFFFF\` at the end)
- \`polynomial_hash(s, base=31, mod=10**9+7)\` — sum of \`ord(c) * base^i\` for each char`,

	starterCode: `def djb2_hash(s):
    # Start with hash = 5381
    # For each char: hash = (hash * 33) ^ ord(c)
    # Return hash & 0xFFFFFFFF
    pass

def polynomial_hash(s, base=31, mod=10**9+7):
    # Sum ord(c) * base^i mod m for i=0,1,...
    pass

print(djb2_hash("hello"))
print(djb2_hash("world"))
print(polynomial_hash("abc"))
`,

	solution: `def djb2_hash(s):
    hash_val = 5381
    for c in s:
        hash_val = (hash_val * 33) ^ ord(c)
    return hash_val & 0xFFFFFFFF

def polynomial_hash(s, base=31, mod=10**9+7):
    result = 0
    power = 1
    for c in s:
        result = (result + ord(c) * power) % mod
        power = (power * base) % mod
    return result

print(djb2_hash("hello"))
print(djb2_hash("world"))
print(polynomial_hash("abc"))
`,

	tests: [
		{
			name: "djb2_hash('hello') = 178056679",
			expected: "178056679\n191451879\n98274\n",
		},
		{
			name: "djb2_hash('') = 5381",
			code: `{{FUNC}}
print(djb2_hash(""))`,
			expected: "5381\n",
		},
		{
			name: "polynomial_hash('hello') = 105835282",
			code: `{{FUNC}}
print(polynomial_hash("hello"))`,
			expected: "105835282\n",
		},
		{
			name: "different strings produce different hashes",
			code: `{{FUNC}}
print(djb2_hash("abc") == djb2_hash("abc"))
print(djb2_hash("abc") == djb2_hash("cba"))`,
			expected: "True\nFalse\n",
		},
	],
};
