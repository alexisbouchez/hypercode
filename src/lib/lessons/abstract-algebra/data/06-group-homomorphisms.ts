import type { Lesson } from "../../types";

export const groupHomomorphisms: Lesson = {
	id: "group-homomorphisms",
	title: "Group Homomorphisms",
	chapterId: "homomorphisms",
	content: `## Group Homomorphisms

A **group homomorphism** is a function $\\phi: G \\to H$ between groups that preserves the group operation:

$$\\phi(a *_G b) = \\phi(a) *_H \\phi(b)$$

### Properties of Homomorphisms

If $\\phi: G \\to H$ is a homomorphism, then:
- $\\phi(e_G) = e_H$ (identity maps to identity)
- $\\phi(a^{-1}) = \\phi(a)^{-1}$ (inverses map to inverses)

### Important Types

- **Isomorphism**: bijective homomorphism (groups are "the same" structurally)
- **Automorphism**: isomorphism from a group to itself
- **Kernel**: $\\ker(\\phi) = \\{a \\in G : \\phi(a) = e_H\\}$ — always a subgroup of $G$

### Example: $\\phi: \\mathbb{Z}_6 \\to \\mathbb{Z}_3$

Define $\\phi(a) = a \\bmod 3$. This is a homomorphism because:
$$(a + b) \\bmod 3 = ((a \\bmod 3) + (b \\bmod 3)) \\bmod 3$$

The kernel is $\\ker(\\phi) = \\{0, 3\\}$ — elements that map to $0$ in $\\mathbb{Z}_3$.

### Checking Homomorphism Computationally

Given groups $\\mathbb{Z}_m$ and $\\mathbb{Z}_n$ and a function $\\phi$, verify:

\`\`\`python
def is_homomorphism(phi, m, n):
    for a in range(m):
        for b in range(m):
            if phi((a + b) % m) != (phi(a) + phi(b)) % n:
                return False
    return True
\`\`\`

### Your Task

Implement \`is_homomorphism(phi, m, n)\` that checks if function \`phi\` (a list where \`phi[a]\` gives $\\phi(a)$) is a homomorphism from $\\mathbb{Z}_m \\to \\mathbb{Z}_n$ under addition, and \`kernel(phi, m)\` that returns the sorted kernel of $\\phi$.`,

	starterCode: `def is_homomorphism(phi, m, n):
    # phi is a list: phi[a] = image of a
    # Check phi((a+b) % m) == (phi[a] + phi[b]) % n for all a, b
    pass

def kernel(phi, m):
    # Return sorted list of elements mapping to 0
    pass

# phi: Z_6 -> Z_3, phi(a) = a % 3
phi1 = [i % 3 for i in range(6)]
print(is_homomorphism(phi1, 6, 3))
print(kernel(phi1, 6))

# Not a homomorphism
phi2 = [0, 1, 0, 1, 0, 1]
print(is_homomorphism(phi2, 6, 3))
`,

	solution: `def is_homomorphism(phi, m, n):
    for a in range(m):
        for b in range(m):
            if phi[(a + b) % m] != (phi[a] + phi[b]) % n:
                return False
    return True

def kernel(phi, m):
    return sorted([a for a in range(m) if phi[a] == 0])

# phi: Z_6 -> Z_3, phi(a) = a % 3
phi1 = [i % 3 for i in range(6)]
print(is_homomorphism(phi1, 6, 3))
print(kernel(phi1, 6))

# Not a homomorphism
phi2 = [0, 1, 0, 1, 0, 1]
print(is_homomorphism(phi2, 6, 3))
`,

	tests: [
		{
			name: "Z_6 -> Z_3 homomorphism and kernel",
			expected: "True\n[0, 3]\nFalse\n",
		},
		{
			name: "Identity map Z_4 -> Z_4 is a homomorphism",
			code: `{{FUNC}}
phi = [0, 1, 2, 3]
print(is_homomorphism(phi, 4, 4))
print(kernel(phi, 4))`,
			expected: "True\n[0]\n",
		},
		{
			name: "Zero map Z_5 -> Z_5 is a homomorphism",
			code: `{{FUNC}}
phi = [0, 0, 0, 0, 0]
print(is_homomorphism(phi, 5, 5))
print(kernel(phi, 5))`,
			expected: "True\n[0, 1, 2, 3, 4]\n",
		},
	],
};
