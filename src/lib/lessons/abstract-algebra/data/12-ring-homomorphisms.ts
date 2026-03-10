import type { Lesson } from "../../types";

export const ringHomomorphisms: Lesson = {
	id: "ring-homomorphisms",
	title: "Ring Homomorphisms",
	chapterId: "rings",
	content: `## Ring Homomorphisms

A **ring homomorphism** $\\phi: R \\to S$ is a function that preserves both operations:

$$\\phi(a + b) = \\phi(a) + \\phi(b)$$
$$\\phi(a \\cdot b) = \\phi(a) \\cdot \\phi(b)$$

If $R$ and $S$ have unity, we also require $\\phi(1_R) = 1_S$.

### The Natural Projection

The map $\\phi: \\mathbb{Z}_m \\to \\mathbb{Z}_n$ defined by $\\phi(a) = a \\bmod n$ is a ring homomorphism whenever $n$ divides $m$.

For example, $\\phi: \\mathbb{Z}_{12} \\to \\mathbb{Z}_4$ with $\\phi(a) = a \\bmod 4$:
- $\\phi(7 + 8) = \\phi(3) = 3$ and $\\phi(7) + \\phi(8) = 3 + 0 = 3$ in $\\mathbb{Z}_4$ ✓
- $\\phi(3 \\cdot 5) = \\phi(3) = 3$ and $\\phi(3) \\cdot \\phi(5) = 3 \\cdot 1 = 3$ in $\\mathbb{Z}_4$ ✓

### Kernel of a Ring Homomorphism

$$\\ker(\\phi) = \\{a \\in R : \\phi(a) = 0_S\\}$$

The kernel is always an ideal of $R$. For $\\phi: \\mathbb{Z}_{12} \\to \\mathbb{Z}_4$, the kernel is $\\{0, 4, 8\\}$.

### Image

$$\\text{im}(\\phi) = \\{\\phi(a) : a \\in R\\}$$

The image is a subring of $S$.

### Your Task

Implement \`is_ring_hom(phi, m, n)\` that checks if a function (given as a list) is a ring homomorphism from $\\mathbb{Z}_m \\to \\mathbb{Z}_n$, \`ring_kernel(phi, m)\` for the kernel, and \`ring_image(phi, m)\` for the image.`,

	starterCode: `def is_ring_hom(phi, m, n):
    # Check both addition and multiplication preservation
    pass

def ring_kernel(phi, m):
    # Elements mapping to 0
    pass

def ring_image(phi, m):
    # All distinct values in the image, sorted
    pass

# Z_12 -> Z_4: a mod 4
phi = [i % 4 for i in range(12)]
print(is_ring_hom(phi, 12, 4))
print(ring_kernel(phi, 12))
print(ring_image(phi, 12))

# Not a ring hom
bad = [0] * 12
print(is_ring_hom(bad, 12, 4))
`,

	solution: `def is_ring_hom(phi, m, n):
    for a in range(m):
        for b in range(m):
            # Check addition
            if phi[(a + b) % m] != (phi[a] + phi[b]) % n:
                return False
            # Check multiplication
            if phi[(a * b) % m] != (phi[a] * phi[b]) % n:
                return False
    return True

def ring_kernel(phi, m):
    return sorted([a for a in range(m) if phi[a] == 0])

def ring_image(phi, m):
    return sorted(set(phi[a] for a in range(m)))

# Z_12 -> Z_4: a mod 4
phi = [i % 4 for i in range(12)]
print(is_ring_hom(phi, 12, 4))
print(ring_kernel(phi, 12))
print(ring_image(phi, 12))

# Not a ring hom (zero map doesn't preserve 1)
bad = [0] * 12
print(is_ring_hom(bad, 12, 4))
`,

	tests: [
		{
			name: "Z_12 -> Z_4 ring hom, kernel, image",
			expected: "True\n[0, 4, 8]\n[0, 1, 2, 3]\nTrue\n",
		},
		{
			name: "Z_6 -> Z_2 natural projection",
			code: `{{FUNC}}
phi = [i % 2 for i in range(6)]
print(is_ring_hom(phi, 6, 2))
print(ring_kernel(phi, 6))`,
			expected: "True\n[0, 2, 4]\n",
		},
		{
			name: "Z_6 -> Z_3 natural projection",
			code: `{{FUNC}}
phi = [i % 3 for i in range(6)]
print(is_ring_hom(phi, 6, 3))
print(ring_image(phi, 6))`,
			expected: "True\n[0, 1, 2]\n",
		},
	],
};
