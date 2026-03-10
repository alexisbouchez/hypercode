import type { Lesson } from "../../types";

export const cosetsLagrange: Lesson = {
	id: "cosets-lagrange",
	title: "Cosets & Lagrange's Theorem",
	chapterId: "homomorphisms",
	content: `## Cosets & Lagrange's Theorem

### Cosets

Let $H$ be a subgroup of $G$. For any element $a \\in G$, the **left coset** of $H$ with respect to $a$ is:

$$aH = \\{a * h : h \\in H\\}$$

In $\\mathbb{Z}_n$, where the operation is addition: $a + H = \\{(a + h) \\bmod n : h \\in H\\}$.

### Example

In $\\mathbb{Z}_6$, let $H = \\{0, 3\\}$. The cosets are:
- $0 + H = \\{0, 3\\}$
- $1 + H = \\{1, 4\\}$
- $2 + H = \\{2, 5\\}$

Notice that every element of $\\mathbb{Z}_6$ belongs to exactly one coset, and all cosets have the same size.

### Lagrange's Theorem

If $H$ is a subgroup of a finite group $G$, then $|H|$ divides $|G|$. Moreover:

$$|G| = |H| \\cdot [G : H]$$

where $[G : H]$ is the **index** of $H$ in $G$ (the number of distinct cosets).

### Consequences

- The order of any element divides $|G|$
- A group of prime order $p$ is cyclic (its only subgroups are $\\{e\\}$ and $G$ itself)
- $a^{|G|} = e$ for all $a \\in G$ (gives Fermat's little theorem as a special case)

### Your Task

Implement \`left_cosets(subgroup, n)\` that returns the list of all distinct left cosets of the given subgroup in $\\mathbb{Z}_n$ (each coset as a sorted list), sorted by smallest element. Also implement \`verify_lagrange(n)\` that checks Lagrange's theorem for all subgroups of $\\mathbb{Z}_n$.`,

	starterCode: `def generated_subgroup(a, n):
    subgroup = set()
    x = 0
    while True:
        subgroup.add(x)
        x = (x + a) % n
        if x in subgroup:
            break
    return sorted(subgroup)

def left_cosets(subgroup, n):
    # Return list of distinct cosets, each sorted, sorted by min element
    pass

def verify_lagrange(n):
    # Check |H| divides n for every subgroup H of Z_n
    pass

cosets = left_cosets([0, 3], 6)
for c in cosets:
    print(c)
print(verify_lagrange(6))
print(verify_lagrange(12))
`,

	solution: `def generated_subgroup(a, n):
    subgroup = set()
    x = 0
    while True:
        subgroup.add(x)
        x = (x + a) % n
        if x in subgroup:
            break
    return sorted(subgroup)

def left_cosets(subgroup, n):
    seen = set()
    cosets = []
    for a in range(n):
        coset = tuple(sorted((a + h) % n for h in subgroup))
        if coset not in seen:
            seen.add(coset)
            cosets.append(list(coset))
    cosets.sort(key=lambda c: c[0])
    return cosets

def verify_lagrange(n):
    seen = set()
    for a in range(n):
        sg = tuple(generated_subgroup(a, n))
        if sg not in seen:
            seen.add(sg)
            if n % len(sg) != 0:
                return False
    return True

cosets = left_cosets([0, 3], 6)
for c in cosets:
    print(c)
print(verify_lagrange(6))
print(verify_lagrange(12))
`,

	tests: [
		{
			name: "Cosets of {0,3} in Z_6 and Lagrange verification",
			expected: "[0, 3]\n[1, 4]\n[2, 5]\nTrue\nTrue\n",
		},
		{
			name: "Cosets of {0,2,4} in Z_6",
			code: `{{FUNC}}
cosets = left_cosets([0, 2, 4], 6)
for c in cosets:
    print(c)`,
			expected: "[0, 2, 4]\n[1, 3, 5]\n",
		},
		{
			name: "Lagrange holds for Z_15",
			code: `{{FUNC}}
print(verify_lagrange(15))`,
			expected: "True\n",
		},
	],
};
