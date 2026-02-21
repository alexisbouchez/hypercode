import type { Lesson } from "../../types";

export const gcdExtended: Lesson = {
	id: "gcd-extended",
	title: "Extended Euclidean Algorithm",
	chapterId: "divisibility",
	content: `## Extended Euclidean Algorithm

The **Extended Euclidean Algorithm** does more than just find $\\gcd(a, b)$. It also finds integers $x$ and $y$ such that:

$$ax + by = \\gcd(a, b)$$

This is known as **Bezout's identity**, and the pair $(x, y)$ is called a **Bezout coefficient** pair. These coefficients are essential for computing modular inverses and solving linear Diophantine equations.

### The Recursion

The algorithm extends the standard Euclidean recursion. If $\\gcd(a, b) = \\gcd(b, a \\bmod b)$, we track coefficients backward:

$$\\gcd(a, b) = g, \\quad bx' + (a \\bmod b)y' = g$$

Then the coefficients for the original $(a, b)$ are:

$$x = y', \\quad y = x' - \\left\\lfloor \\frac{a}{b} \\right\\rfloor y'$$

\`\`\`python
def extended_gcd(a, b):
    if b == 0:
        return a, 1, 0      # gcd=a, x=1, y=0 since a*1 + 0*0 = a
    g, x, y = extended_gcd(b, a % b)
    return g, y, x - (a // b) * y

g, x, y = extended_gcd(35, 15)
print(g, x, y)   # 5 1 -2
# Verify: 35*1 + 15*(-2) = 35 - 30 = 5 ✓
\`\`\`

### Applications

- **Modular inverse**: if $\\gcd(a, m) = 1$ then $x$ from $\\text{extended\\_gcd}(a, m)$ gives $a^{-1} \\pmod{m}$
- **Chinese Remainder Theorem**: solve systems of congruences
- **Linear Diophantine equations**: $ax + by = c$ has integer solutions iff $\\gcd(a,b) \\mid c$

### Your Task

Implement \`extended_gcd(a, b)\` that returns a tuple \`(g, x, y)\` where $g = \\gcd(a, b)$ and $ax + by = g$.`,

	starterCode: `def extended_gcd(a, b):
    # Base case: if b == 0, return (a, 1, 0)
    # Recursive case: recurse on (b, a % b), then adjust coefficients
    pass

g, x, y = extended_gcd(35, 15)
print(g, x, y)
`,

	solution: `def extended_gcd(a, b):
    if b == 0:
        return a, 1, 0
    g, x, y = extended_gcd(b, a % b)
    return g, y, x - (a // b) * y

g, x, y = extended_gcd(35, 15)
print(g, x, y)
`,

	tests: [
		{
			name: "extended_gcd(35, 15) = (5, 1, -2)",
			expected: "5 1 -2\n",
		},
		{
			name: "extended_gcd(3, 5): gcd=1 and 3x+5y=1",
			code: `{{FUNC}}
g, x, y = extended_gcd(3, 5)
print(g)
print(3*x + 5*y)`,
			expected: "1\n1\n",
		},
		{
			name: "extended_gcd(30, 20): gcd=10",
			code: `{{FUNC}}
g, x, y = extended_gcd(30, 20)
print(g)
print(30*x + 20*y)`,
			expected: "10\n10\n",
		},
		{
			name: "extended_gcd(7, 13): coprime, Bezout identity holds",
			code: `{{FUNC}}
g, x, y = extended_gcd(7, 13)
print(g)
print(7*x + 13*y)`,
			expected: "1\n1\n",
		},
	],
};
