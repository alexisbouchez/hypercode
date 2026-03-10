import type { Lesson } from "../../types";

export const fields: Lesson = {
	id: "fields",
	title: "Fields",
	chapterId: "fields",
	content: `## Fields

A **field** $(F, +, \\cdot)$ is a commutative ring with unity where every nonzero element has a multiplicative inverse. Equivalently:

1. $(F, +)$ is an abelian group with identity $0$
2. $(F \\setminus \\{0\\}, \\cdot)$ is an abelian group with identity $1$
3. Multiplication distributes over addition

### $\\mathbb{Z}_p$ is a Field

When $p$ is prime, $\\mathbb{Z}_p$ is a field. Every nonzero element $a$ has a multiplicative inverse, computable by:

$$a^{-1} \\equiv a^{p-2} \\pmod{p}$$

This follows from **Fermat's little theorem**: $a^{p-1} \\equiv 1 \\pmod{p}$.

\`\`\`python
def mod_inverse(a, p):
    # Using Fermat's little theorem
    return pow(a, p - 2, p)
\`\`\`

### Why Composites Don't Work

$\\mathbb{Z}_6$ is **not** a field because $2 \\cdot 3 = 0$ — zero divisors exist. An element has a multiplicative inverse if and only if it is coprime to $n$.

### Field Multiplication Table

For $\\mathbb{Z}_5$:

| $\\cdot$ | 1 | 2 | 3 | 4 |
|---|---|---|---|---|
| 1 | 1 | 2 | 3 | 4 |
| 2 | 2 | 4 | 1 | 3 |
| 3 | 3 | 1 | 4 | 2 |
| 4 | 4 | 3 | 2 | 1 |

Every row is a permutation of $\\{1,2,3,4\\}$ — this is a hallmark of a field.

### Your Task

Implement \`is_field(n)\` that checks if $\\mathbb{Z}_n$ is a field, \`mod_inverse(a, p)\` computing $a^{-1}$ in $\\mathbb{Z}_p$, and \`mul_table(p)\` returning the multiplication table for the nonzero elements of $\\mathbb{Z}_p$.`,

	starterCode: `def is_field(n):
    # Z_n is a field iff n is prime
    pass

def mod_inverse(a, p):
    # Compute a^{-1} mod p using Fermat's little theorem
    pass

def mul_table(p):
    # Return multiplication table for {1, ..., p-1} in Z_p
    pass

print(is_field(5))
print(is_field(6))
print(mod_inverse(3, 7))
table = mul_table(5)
for row in table:
    print(row)
`,

	solution: `def is_field(n):
    if n < 2:
        return False
    for i in range(2, n):
        if i * i > n:
            break
        if n % i == 0:
            return False
    return True

def mod_inverse(a, p):
    result = 1
    base = a % p
    exp = p - 2
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % p
        exp //= 2
        base = (base * base) % p
    return result

def mul_table(p):
    return [[(i * j) % p for j in range(1, p)] for i in range(1, p)]

print(is_field(5))
print(is_field(6))
print(mod_inverse(3, 7))
table = mul_table(5)
for row in table:
    print(row)
`,

	tests: [
		{
			name: "Field check, inverse, and mul table",
			expected:
				"True\nFalse\n5\n[1, 2, 3, 4]\n[2, 4, 1, 3]\n[3, 1, 4, 2]\n[4, 3, 2, 1]\n",
		},
		{
			name: "mod_inverse(2, 11) = 6",
			code: `{{FUNC}}
print(mod_inverse(2, 11))
print((2 * mod_inverse(2, 11)) % 11)`,
			expected: "6\n1\n",
		},
		{
			name: "Primes are fields, composites are not",
			code: `{{FUNC}}
for n in [2, 3, 4, 7, 9, 11]:
    print(n, is_field(n))`,
			expected: "2 True\n3 True\n4 False\n7 True\n9 False\n11 True\n",
		},
	],
};
