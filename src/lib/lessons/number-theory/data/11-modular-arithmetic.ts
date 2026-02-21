import type { Lesson } from "../../types";

export const modularArithmetic: Lesson = {
	id: "modular-arithmetic",
	title: "Modular Arithmetic",
	chapterId: "modular",
	content: `## Modular Arithmetic

**Modular arithmetic** is arithmetic on a "clock" — numbers wrap around after reaching a modulus $m$. We write:

$$a \\equiv b \\pmod{m}$$

which means $m \\mid (a - b)$, i.e., $a$ and $b$ leave the same remainder when divided by $m$.

### Basic Operations

Modular arithmetic is closed under addition and multiplication:

$$\\begin{align}
(a + b) \\bmod m &= ((a \\bmod m) + (b \\bmod m)) \\bmod m \\\\
(a \\cdot b) \\bmod m &= ((a \\bmod m) \\cdot (b \\bmod m)) \\bmod m
\\end{align}$$

\`\`\`python
def mod_add(a, b, m):
    return (a + b) % m

def mod_mul(a, b, m):
    return (a * b) % m
\`\`\`

### Fast Modular Exponentiation

Computing $a^n \\pmod{m}$ by repeated squaring runs in $O(\\log n)$ instead of $O(n)$:

$$a^n = \\begin{cases} 1 & \\text{if } n = 0 \\\\ (a^{n/2})^2 & \\text{if } n \\text{ even} \\\\ a \\cdot a^{n-1} & \\text{if } n \\text{ odd} \\end{cases}$$

\`\`\`python
def mod_pow(base, exp, m):
    result = 1
    base = base % m
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % m
        exp //= 2
        base = (base * base) % m
    return result

print(mod_pow(2, 10, 1000))  # 1024 % 1000 = 24
\`\`\`

This iterative approach processes each bit of \`exp\` from right to left, squaring \`base\` each iteration and multiplying into \`result\` when the bit is 1.

### Applications

Modular exponentiation is the heart of:
- **RSA encryption**: $c = m^e \\bmod n$
- **Diffie-Hellman key exchange**: $g^a \\bmod p$
- **Primality testing**: Fermat and Miller-Rabin tests

### Your Task

Implement \`mod_add(a, b, m)\`, \`mod_mul(a, b, m)\`, and \`mod_pow(base, exp, m)\`. Do **not** use Python's built-in 3-argument \`pow()\` for \`mod_pow\` — implement it yourself with the squaring loop.`,

	starterCode: `def mod_add(a, b, m):
    return (a + b) % m

def mod_mul(a, b, m):
    return (a * b) % m

def mod_pow(base, exp, m):
    # Implement fast exponentiation by repeated squaring
    pass

print(mod_add(7, 8, 5))
print(mod_mul(4, 7, 5))
print(mod_pow(2, 10, 1000))
`,

	solution: `def mod_add(a, b, m):
    return (a + b) % m

def mod_mul(a, b, m):
    return (a * b) % m

def mod_pow(base, exp, m):
    result = 1
    base = base % m
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % m
        exp //= 2
        base = (base * base) % m
    return result

print(mod_add(7, 8, 5))
print(mod_mul(4, 7, 5))
print(mod_pow(2, 10, 1000))
`,

	tests: [
		{
			name: "mod_add(7,8,5)=0, mod_mul(4,7,5)=3, mod_pow(2,10,1000)=24",
			expected: "0\n3\n24\n",
		},
		{
			name: "mod_pow(3, 4, 7) = 4",
			code: `{{FUNC}}
print(mod_pow(3, 4, 7))`,
			expected: "4\n",
		},
		{
			name: "mod_pow(5, 3, 13) = 8",
			code: `{{FUNC}}
print(mod_pow(5, 3, 13))`,
			expected: "8\n",
		},
		{
			name: "mod_add(999, 1, 1000) = 0",
			code: `{{FUNC}}
print(mod_add(999, 1, 1000))`,
			expected: "0\n",
		},
		{
			name: "mod_mul(123, 456, 100) = 88",
			code: `{{FUNC}}
print(mod_mul(123, 456, 100))`,
			expected: "88\n",
		},
	],
};
