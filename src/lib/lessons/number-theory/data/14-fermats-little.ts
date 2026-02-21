import type { Lesson } from "../../types";

export const fermatsLittle: Lesson = {
	id: "fermats-little",
	title: "Fermat's Little Theorem",
	chapterId: "modular",
	content: `## Fermat's Little Theorem

**Fermat's Little Theorem** (proved by Fermat in 1640, published by Euler in 1736) states:

> If $p$ is prime and $\\gcd(a, p) = 1$ (i.e., $p \\nmid a$), then:
> $$a^{p-1} \\equiv 1 \\pmod{p}$$

For example, with $a = 2$ and $p = 7$:
$$2^6 = 64 = 9 \\cdot 7 + 1 \\equiv 1 \\pmod{7}$$

### Fermat Primality Test

This gives a fast **probabilistic primality test**: if $a^{p-1} \\not\\equiv 1 \\pmod{p}$ for some $a$, then $p$ is definitely **not** prime. If $a^{p-1} \\equiv 1 \\pmod{p}$, then $p$ is probably prime (but may be a **Carmichael number**).

\`\`\`python
def fermat_test(a, p):
    return pow(a, p - 1, p) == 1

print(fermat_test(2, 7))   # True  (7 is prime)
print(fermat_test(2, 11))  # True  (11 is prime)
\`\`\`

### Modular Inverse via Fermat

Fermat's theorem gives us a way to compute **modular inverses** when $p$ is prime:

$$a^{-1} \\equiv a^{p-2} \\pmod{p}$$

This is because $a \\cdot a^{p-2} = a^{p-1} \\equiv 1 \\pmod{p}$.

\`\`\`python
def mod_inverse_fermat(a, p):
    return pow(a, p - 2, p)

# 3^(-1) mod 7: 3 * ? ≡ 1 (mod 7)
print(mod_inverse_fermat(3, 7))   # 5  (since 3*5=15≡1 mod 7)
\`\`\`

### When Fermat's Test Fails

**Carmichael numbers** are composite numbers $n$ where $a^{n-1} \\equiv 1 \\pmod{n}$ for all $a$ coprime to $n$. The smallest is 561 = 3 × 11 × 17. For reliable primality testing, use the **Miller-Rabin test** instead.

### Your Task

Implement:
- \`fermat_test(a, p)\` → \`True\` if $a^{p-1} \\equiv 1 \\pmod{p}$
- \`mod_inverse_fermat(a, p)\` → $a^{p-2} \\bmod p$ (modular inverse when $p$ is prime)

You may use Python's built-in \`pow(a, exp, mod)\` for these.`,

	starterCode: `def fermat_test(a, p):
    # Return True if a^(p-1) % p == 1
    pass

def mod_inverse_fermat(a, p):
    # Return a^(p-2) % p (Fermat inverse, p must be prime)
    pass

print(fermat_test(2, 7))
print(mod_inverse_fermat(3, 7))
`,

	solution: `def fermat_test(a, p):
    return pow(a, p - 1, p) == 1

def mod_inverse_fermat(a, p):
    return pow(a, p - 2, p)

print(fermat_test(2, 7))
print(mod_inverse_fermat(3, 7))
`,

	tests: [
		{
			name: "fermat_test(2, 7) = True, mod_inverse_fermat(3, 7) = 5",
			expected: "True\n5\n",
		},
		{
			name: "fermat_test(3, 7) = True",
			code: `{{FUNC}}
print(fermat_test(3, 7))`,
			expected: "True\n",
		},
		{
			name: "fermat_test(2, 11) = True",
			code: `{{FUNC}}
print(fermat_test(2, 11))`,
			expected: "True\n",
		},
		{
			name: "mod_inverse_fermat(5, 11) = 9 (since 5*9=45≡1 mod 11)",
			code: `{{FUNC}}
print(mod_inverse_fermat(5, 11))`,
			expected: "9\n",
		},
		{
			name: "verify inverse: a * mod_inverse_fermat(a,p) ≡ 1 (mod p)",
			code: `{{FUNC}}
a, p = 7, 13
inv = mod_inverse_fermat(a, p)
print((a * inv) % p)`,
			expected: "1\n",
		},
	],
};
