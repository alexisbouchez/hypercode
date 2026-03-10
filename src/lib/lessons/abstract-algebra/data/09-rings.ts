import type { Lesson } from "../../types";

export const rings: Lesson = {
	id: "rings",
	title: "Rings",
	chapterId: "rings",
	content: `## Rings

A **ring** $(R, +, \\cdot)$ is a set $R$ with two binary operations satisfying:

1. $(R, +)$ is an abelian group
2. Multiplication is associative: $(a \\cdot b) \\cdot c = a \\cdot (b \\cdot c)$
3. Distributive laws hold:
   - $a \\cdot (b + c) = a \\cdot b + a \\cdot c$
   - $(a + b) \\cdot c = a \\cdot c + b \\cdot c$

If multiplication is commutative, we call it a **commutative ring**. If there is a multiplicative identity $1$, we call it a **ring with unity**.

### Example: $\\mathbb{Z}_n$

The integers modulo $n$ form a commutative ring with unity under addition and multiplication mod $n$.

\`\`\`python
def Zn_add(a, b, n):
    return (a + b) % n

def Zn_mul(a, b, n):
    return (a * b) % n
\`\`\`

### Zero Divisors

An element $a \\neq 0$ is a **zero divisor** if there exists $b \\neq 0$ such that $a \\cdot b = 0$.

In $\\mathbb{Z}_6$: $2 \\cdot 3 = 0$, so both 2 and 3 are zero divisors.

In $\\mathbb{Z}_5$ (prime modulus): there are no zero divisors — this makes it an **integral domain**.

### Units

An element $a$ is a **unit** (invertible) if there exists $b$ such that $a \\cdot b = 1 \\pmod{n}$. The units form a group under multiplication, denoted $\\mathbb{Z}_n^*$.

### Your Task

Implement \`zero_divisors(n)\` returning the sorted list of zero divisors in $\\mathbb{Z}_n$, and \`units(n)\` returning the sorted list of units (multiplicatively invertible elements).`,

	starterCode: `def zero_divisors(n):
    # Return sorted list of zero divisors in Z_n
    pass

def units(n):
    # Return sorted list of units in Z_n
    pass

print(zero_divisors(6))
print(units(6))
print(zero_divisors(7))
print(units(7))
`,

	solution: `def zero_divisors(n):
    zd = set()
    for a in range(1, n):
        for b in range(1, n):
            if (a * b) % n == 0:
                zd.add(a)
    return sorted(zd)

def units(n):
    result = []
    for a in range(1, n):
        for b in range(1, n):
            if (a * b) % n == 1:
                result.append(a)
                break
    return sorted(result)

print(zero_divisors(6))
print(units(6))
print(zero_divisors(7))
print(units(7))
`,

	tests: [
		{
			name: "Zero divisors and units of Z_6 and Z_7",
			expected: "[2, 3, 4]\n[1, 5]\n[]\n[1, 2, 3, 4, 5, 6]\n",
		},
		{
			name: "Z_8 zero divisors",
			code: `{{FUNC}}
print(zero_divisors(8))`,
			expected: "[2, 4, 6]\n",
		},
		{
			name: "Z_8 units",
			code: `{{FUNC}}
print(units(8))`,
			expected: "[1, 3, 5, 7]\n",
		},
		{
			name: "Z_5 (prime) has no zero divisors",
			code: `{{FUNC}}
print(zero_divisors(5))
print(len(units(5)))`,
			expected: "[]\n4\n",
		},
	],
};
