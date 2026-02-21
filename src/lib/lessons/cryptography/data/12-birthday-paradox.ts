import type { Lesson } from "../../types";

export const birthdayParadox: Lesson = {
	id: "birthday-paradox",
	title: "Birthday Paradox & Hash Collisions",
	chapterId: "hash",
	content: `## Birthday Paradox and Hash Collisions

How many people must be in a room before two of them share a birthday? Surprisingly, only **23** people are needed for a >50% chance. This counter-intuitive result — the **birthday paradox** — directly determines the security of hash functions.

### Birthday Attack on Hash Functions

If a hash function produces $n$-bit outputs ($2^n$ possible values), how many random inputs do we need before finding two with the same hash?

The birthday paradox gives us:

$$P(\\text{collision after } k \\text{ hashes}) \\approx 1 - e^{-k(k-1)/(2 \\cdot 2^n)}$$

For a 50% collision probability, the threshold is approximately:

$$k \\approx \\sqrt{2 \\cdot 2^n \\cdot \\ln 2} \\approx 1.177 \\cdot 2^{n/2}$$

### Implications for Hash Security

| Hash size | Collision threshold | Security level |
|-----------|---------------------|----------------|
| 32 bits   | ~77,163             | Toy — trivially broken |
| 64 bits   | ~5 billion          | Weak — birthday in seconds |
| 128 bits  | ~$2^{64}$           | MD5 range — compromised |
| 256 bits  | ~$2^{128}$          | SHA-256 — considered secure |

**Rule of thumb**: an $n$-bit hash provides only $n/2$ bits of collision resistance.

### Your Task

Implement:
- \`birthday_probability(n, hash_bits)\` — probability of at least one collision among $n$ hashes of size \`hash_bits\` bits: $1 - e^{-n(n-1)/(2 \\cdot 2^{\\text{hash\\_bits}})}$
- \`collision_threshold(hash_bits, probability=0.5)\` — minimum $n$ for collision probability $\\geq p$: $\\lceil \\sqrt{2 \\cdot 2^{\\text{hash\\_bits}} \\cdot \\ln(1/(1-p))} \\rceil$

Use \`import math\` for \`math.exp\`, \`math.log\`, \`math.sqrt\`, \`math.ceil\`.`,

	starterCode: `import math

def birthday_probability(n, hash_bits):
    # P = 1 - exp(-n*(n-1) / (2 * 2**hash_bits))
    pass

def collision_threshold(hash_bits, probability=0.5):
    # k = ceil(sqrt(2 * 2**hash_bits * ln(1/(1-probability))))
    pass

print(round(birthday_probability(1000, 16), 6))
print(collision_threshold(16))
print(collision_threshold(32))
`,

	solution: `import math

def birthday_probability(n, hash_bits):
    return 1 - math.exp(-n * (n - 1) / (2 * 2**hash_bits))

def collision_threshold(hash_bits, probability=0.5):
    return math.ceil(math.sqrt(2 * 2**hash_bits * math.log(1 / (1 - probability))))

print(round(birthday_probability(1000, 16), 6))
print(collision_threshold(16))
print(collision_threshold(32))
`,

	tests: [
		{
			name: "birthday_probability(1000, 16) ≈ 0.99951",
			expected: "0.99951\n302\n77163\n",
		},
		{
			name: "birthday_probability(2, 32) is tiny",
			code: `{{FUNC}}
print(round(birthday_probability(2, 32), 10))`,
			expected: "2e-10\n",
		},
		{
			name: "collision_threshold gives ~50% collision probability",
			code: `{{FUNC}}
k = collision_threshold(16)
p = birthday_probability(k, 16)
print(p >= 0.5)`,
			expected: "True\n",
		},
		{
			name: "birthday_probability(1, hash_bits) = 0",
			code: `{{FUNC}}
print(round(birthday_probability(1, 64), 6))`,
			expected: "0.0\n",
		},
	],
};
