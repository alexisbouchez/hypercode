import type { Lesson } from "../../types";

export const subgroups: Lesson = {
	id: "subgroups",
	title: "Subgroups",
	chapterId: "groups",
	content: `## Subgroups

A **subgroup** $H$ of a group $(G, *)$ is a subset $H \\subseteq G$ that is itself a group under the same operation.

### Subgroup Test

A non-empty subset $H$ of $G$ is a subgroup if and only if for all $a, b \\in H$:
$$a * b^{-1} \\in H$$

This single condition captures closure, identity, and inverses simultaneously.

### Example: Subgroups of $\\mathbb{Z}_6$

The group $\\mathbb{Z}_6 = \\{0,1,2,3,4,5\\}$ under addition mod 6 has these subgroups:
- $\\{0\\}$ — trivial subgroup
- $\\{0, 3\\}$ — order 2
- $\\{0, 2, 4\\}$ — order 3
- $\\{0, 1, 2, 3, 4, 5\\}$ — the whole group

Notice that the subgroup orders (1, 2, 3, 6) all divide $|G| = 6$. This is **Lagrange's theorem**, which we will prove later.

### Finding Subgroups Computationally

For $\\mathbb{Z}_n$ under addition, a subset $H$ is a subgroup if:
1. $0 \\in H$ (identity)
2. For each $a \\in H$, $(n - a) \\% n \\in H$ (inverses)
3. For each $a, b \\in H$, $(a + b) \\% n \\in H$ (closure)

### Generated Subgroups

The subgroup **generated** by an element $a$, written $\\langle a \\rangle$, is the smallest subgroup containing $a$:

$$\\langle a \\rangle = \\{e, a, a^2, a^3, \\ldots\\}$$

In $\\mathbb{Z}_n$, $\\langle a \\rangle = \\{0, a, 2a, 3a, \\ldots\\} \\pmod{n}$.

### Your Task

Implement \`generated_subgroup(a, n)\` that returns the sorted list of elements in $\\langle a \\rangle$ within $\\mathbb{Z}_n$, and \`find_all_subgroups(n)\` that returns all subgroups of $\\mathbb{Z}_n$ (each as a sorted list), sorted by size then lexicographically.`,

	starterCode: `def generated_subgroup(a, n):
    # Return sorted list of elements in <a> within Z_n
    pass

def find_all_subgroups(n):
    # Return all subgroups of Z_n, sorted by size then lex
    pass

print(generated_subgroup(2, 6))
print(generated_subgroup(3, 6))
subs = find_all_subgroups(6)
for s in subs:
    print(s)
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

def find_all_subgroups(n):
    seen = set()
    result = []
    for a in range(n):
        sg = tuple(generated_subgroup(a, n))
        if sg not in seen:
            seen.add(sg)
            result.append(list(sg))
    result.sort(key=lambda s: (len(s), s))
    return result

print(generated_subgroup(2, 6))
print(generated_subgroup(3, 6))
subs = find_all_subgroups(6)
for s in subs:
    print(s)
`,

	tests: [
		{
			name: "Generated subgroups of Z_6 and all subgroups",
			expected:
				"[0, 2, 4]\n[0, 3]\n[0]\n[0, 3]\n[0, 2, 4]\n[0, 1, 2, 3, 4, 5]\n",
		},
		{
			name: "Generated subgroup <1> in Z_5 is all of Z_5",
			code: `{{FUNC}}
print(generated_subgroup(1, 5))`,
			expected: "[0, 1, 2, 3, 4]\n",
		},
		{
			name: "Subgroups of Z_4",
			code: `{{FUNC}}
subs = find_all_subgroups(4)
for s in subs:
    print(s)`,
			expected: "[0]\n[0, 2]\n[0, 1, 2, 3]\n",
		},
	],
};
