import type { Lesson } from "../../types";

export const chineseRemainder: Lesson = {
	id: "chinese-remainder",
	title: "Chinese Remainder Theorem",
	chapterId: "modular",
	content: `## Chinese Remainder Theorem

The **Chinese Remainder Theorem** (CRT) solves systems of simultaneous congruences. Given:

$$x \\equiv a_1 \\pmod{m_1}$$
$$x \\equiv a_2 \\pmod{m_2}$$

when $\\gcd(m_1, m_2) = 1$, there is a **unique** solution $x$ modulo $m_1 m_2$.

### Historical Context

The theorem is named after the "Mathematical Classic of Sun Zi" (4th century AD), which posed: "We have things of which we do not know the number; if we count them by threes, we have two left over; by fives, we have three left over; by sevens, we have two left over. How many things are there?"

### Solving with Extended GCD

Using Bezout's identity: find $s, t$ such that $m_1 s + m_2 t = 1$ (using extended GCD). Then:

$$x = a_1 m_2 t + a_2 m_1 s \\pmod{m_1 m_2}$$

Equivalently, we can write it as:

$$x = a_1 + m_1 \\cdot s \\cdot (a_2 - a_1) \\pmod{m_1 m_2}$$

\`\`\`python
def extended_gcd(a, b):
    if b == 0:
        return a, 1, 0
    g, x, y = extended_gcd(b, a % b)
    return g, y, x - (a // b) * y

def crt(a1, m1, a2, m2):
    g, s, t = extended_gcd(m1, m2)
    modulus = m1 * m2 // g
    x = (a1 + m1 * s * (a2 - a1) // g) % modulus
    return x

# x ≡ 2 (mod 3), x ≡ 3 (mod 5) → x = 8
print(crt(2, 3, 3, 5))   # 8
# Verify: 8 % 3 = 2 ✓, 8 % 5 = 3 ✓
\`\`\`

### Verification

Always verify: if \`x = crt(a1, m1, a2, m2)\` then \`x % m1 == a1\` and \`x % m2 == a2\`.

### Generalization

CRT extends to any number of congruences, applying the two-congruence version iteratively. It also works when $\\gcd(m_1, m_2) \\neq 1$, but only if $\\gcd(m_1, m_2) \\mid (a_2 - a_1)$.

### Your Task

Using an \`extended_gcd\` helper provided in the starter code, implement \`crt(a1, m1, a2, m2)\` that returns the unique $x \\in [0, m_1 m_2)$ satisfying both congruences.`,

	starterCode: `def extended_gcd(a, b):
    if b == 0:
        return a, 1, 0
    g, x, y = extended_gcd(b, a % b)
    return g, y, x - (a // b) * y

def crt(a1, m1, a2, m2):
    # Find x in [0, m1*m2) with x ≡ a1 (mod m1) and x ≡ a2 (mod m2)
    pass

print(crt(2, 3, 3, 5))
`,

	solution: `def extended_gcd(a, b):
    if b == 0:
        return a, 1, 0
    g, x, y = extended_gcd(b, a % b)
    return g, y, x - (a // b) * y

def crt(a1, m1, a2, m2):
    g, s, t = extended_gcd(m1, m2)
    modulus = m1 * m2 // g
    x = (a1 + m1 * s * (a2 - a1) // g) % modulus
    return x

print(crt(2, 3, 3, 5))
`,

	tests: [
		{
			name: "crt(2, 3, 3, 5) = 8",
			expected: "8\n",
		},
		{
			name: "crt(1, 4, 3, 7): x≡1(mod4), x≡3(mod7) → x=17",
			code: `{{FUNC}}
print(crt(1, 4, 3, 7))`,
			expected: "17\n",
		},
		{
			name: "crt(0, 3, 1, 4): x≡0(mod3), x≡1(mod4) → x=9",
			code: `{{FUNC}}
print(crt(0, 3, 1, 4))`,
			expected: "9\n",
		},
		{
			name: "verify crt(2,3,3,5): result % 3 == 2 and % 5 == 3",
			code: `{{FUNC}}
x = crt(2, 3, 3, 5)
print(x % 3)
print(x % 5)`,
			expected: "2\n3\n",
		},
		{
			name: "verify crt(1,4,3,7): result % 4 == 1 and % 7 == 3",
			code: `{{FUNC}}
x = crt(1, 4, 3, 7)
print(x % 4)
print(x % 7)`,
			expected: "1\n3\n",
		},
	],
};
