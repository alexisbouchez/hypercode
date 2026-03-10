import type { Lesson } from "../../types";

export const finiteFields: Lesson = {
	id: "finite-fields",
	title: "Finite Fields",
	chapterId: "fields",
	content: `## Finite Fields

A **finite field** (or **Galois field**) $\\text{GF}(q)$ has exactly $q$ elements, where $q$ must be a prime power: $q = p^k$.

### $\\text{GF}(p)$ — Prime Fields

When $k = 1$, $\\text{GF}(p) = \\mathbb{Z}_p$. These are the simplest finite fields.

### Multiplicative Group Structure

The nonzero elements of $\\text{GF}(p)$ form a **cyclic** group under multiplication. A **primitive root** (generator of this group) $g$ satisfies:

$$\\{g^1, g^2, \\ldots, g^{p-1}\\} = \\{1, 2, \\ldots, p-1\\}$$

### Finding Primitive Roots

An element $g$ is a primitive root mod $p$ if and only if $g^{(p-1)/q} \\not\\equiv 1 \\pmod{p}$ for every prime factor $q$ of $p - 1$.

### Discrete Logarithm

Given a primitive root $g$, the **discrete logarithm** $\\log_g(a)$ is the unique $k$ such that $g^k \\equiv a \\pmod{p}$.

\`\`\`python
def discrete_log(g, a, p):
    power = 1
    for k in range(p - 1):
        if power == a:
            return k
        power = (power * g) % p
    return -1
\`\`\`

The difficulty of computing discrete logarithms in large fields is the basis of many cryptographic systems.

### Your Task

Implement \`is_primitive_root(g, p)\`, \`find_primitive_roots(p)\` returning all sorted primitive roots, and \`discrete_log(g, a, p)\`.`,

	starterCode: `def is_primitive_root(g, p):
    # Check if g generates all of {1, ..., p-1} under mult mod p
    pass

def find_primitive_roots(p):
    # Return sorted list of all primitive roots mod p
    pass

def discrete_log(g, a, p):
    # Find k such that g^k = a mod p, return -1 if not found
    pass

print(is_primitive_root(2, 7))
print(is_primitive_root(3, 7))
print(find_primitive_roots(7))
print(discrete_log(3, 6, 7))
`,

	solution: `def is_primitive_root(g, p):
    seen = set()
    power = 1
    for _ in range(p - 1):
        power = (power * g) % p
        seen.add(power)
    return len(seen) == p - 1

def find_primitive_roots(p):
    return sorted([g for g in range(1, p) if is_primitive_root(g, p)])

def discrete_log(g, a, p):
    power = 1
    for k in range(p - 1):
        if power == a:
            return k
        power = (power * g) % p
    return -1

print(is_primitive_root(2, 7))
print(is_primitive_root(3, 7))
print(find_primitive_roots(7))
print(discrete_log(3, 6, 7))
`,

	tests: [
		{
			name: "Primitive roots of GF(7) and discrete log",
			expected: "False\nTrue\n[3, 5]\n3\n",
		},
		{
			name: "Primitive roots of GF(11)",
			code: `{{FUNC}}
print(find_primitive_roots(11))`,
			expected: "[2, 6, 7, 8]\n",
		},
		{
			name: "Discrete log: 2^k = 8 mod 13",
			code: `{{FUNC}}
print(discrete_log(2, 8, 13))`,
			expected: "3\n",
		},
		{
			name: "All elements reachable from primitive root",
			code: `{{FUNC}}
g = 3
p = 7
elems = sorted(pow(g, k, p) for k in range(p - 1))
print(elems)`,
			expected: "[1, 2, 3, 4, 5, 6]\n",
		},
	],
};
