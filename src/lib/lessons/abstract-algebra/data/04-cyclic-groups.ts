import type { Lesson } from "../../types";

export const cyclicGroups: Lesson = {
	id: "cyclic-groups",
	title: "Cyclic Groups",
	chapterId: "groups",
	content: `## Cyclic Groups

A group $G$ is **cyclic** if there exists an element $g \\in G$ such that every element of $G$ can be written as a power of $g$:

$$G = \\langle g \\rangle = \\{g^0, g^1, g^2, \\ldots\\}$$

The element $g$ is called a **generator** of $G$.

### Order of an Element

The **order** of an element $a$ in a group $G$, written $\\text{ord}(a)$, is the smallest positive integer $k$ such that $a^k = e$ (identity).

In $\\mathbb{Z}_n$ under addition, the order of $a$ is:

$$\\text{ord}(a) = \\frac{n}{\\gcd(a, n)}$$

### Generators of $\\mathbb{Z}_n$

An element $a$ is a generator of $\\mathbb{Z}_n$ if and only if $\\gcd(a, n) = 1$. The number of generators equals $\\varphi(n)$ (Euler's totient function).

\`\`\`python
from math import gcd

def order_in_Zn(a, n):
    return n // gcd(a, n)

def generators_of_Zn(n):
    return [a for a in range(n) if gcd(a, n) == 1]
\`\`\`

### Fundamental Theorem of Cyclic Groups

Every subgroup of a cyclic group is cyclic. Furthermore, for each divisor $d$ of $n$, there is exactly one subgroup of $\\mathbb{Z}_n$ of order $d$.

### Example: $\\mathbb{Z}_8$

- Generators: $\\{1, 3, 5, 7\\}$ (elements coprime to 8)
- Orders: $\\text{ord}(1) = 8$, $\\text{ord}(2) = 4$, $\\text{ord}(4) = 2$, $\\text{ord}(0) = 1$

### Your Task

Implement \`element_order(a, n)\` for the order of $a$ in $\\mathbb{Z}_n$, \`is_cyclic_generator(a, n)\` to check if $a$ generates $\\mathbb{Z}_n$, and \`all_generators(n)\` returning the sorted list of all generators.`,

	starterCode: `def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def element_order(a, n):
    # Return the order of a in Z_n
    pass

def is_cyclic_generator(a, n):
    # Return True if a generates Z_n
    pass

def all_generators(n):
    # Return sorted list of all generators of Z_n
    pass

print(element_order(2, 8))
print(element_order(3, 8))
print(all_generators(8))
print(all_generators(12))
`,

	solution: `def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def element_order(a, n):
    return n // gcd(a, n)

def is_cyclic_generator(a, n):
    return gcd(a, n) == 1

def all_generators(n):
    return [a for a in range(n) if is_cyclic_generator(a, n)]

print(element_order(2, 8))
print(element_order(3, 8))
print(all_generators(8))
print(all_generators(12))
`,

	tests: [
		{
			name: "Orders and generators of Z_8 and Z_12",
			expected: "4\n8\n[1, 3, 5, 7]\n[1, 5, 7, 11]\n",
		},
		{
			name: "element_order(4, 12) = 3",
			code: `{{FUNC}}
print(element_order(4, 12))`,
			expected: "3\n",
		},
		{
			name: "Generators of Z_7 (prime, so 1..6)",
			code: `{{FUNC}}
print(all_generators(7))`,
			expected: "[1, 2, 3, 4, 5, 6]\n",
		},
		{
			name: "element_order(0, 5) = 1 (identity)",
			code: `{{FUNC}}
print(element_order(0, 5))`,
			expected: "1\n",
		},
	],
};
