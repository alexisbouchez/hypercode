import type { Lesson } from "../../types";

export const quotientGroups: Lesson = {
	id: "quotient-groups",
	title: "Quotient Groups",
	chapterId: "homomorphisms",
	content: `## Quotient Groups

### Normal Subgroups

A subgroup $N$ of $G$ is **normal** (written $N \\trianglelefteq G$) if $aNa^{-1} = N$ for all $a \\in G$. In an abelian group, every subgroup is normal.

Since $\\mathbb{Z}_n$ is abelian, all subgroups of $\\mathbb{Z}_n$ are normal.

### The Quotient Group

If $N \\trianglelefteq G$, the **quotient group** $G/N$ is the set of cosets of $N$ in $G$ with the operation:

$$(aN)(bN) = (ab)N$$

This is well-defined precisely because $N$ is normal.

### Example: $\\mathbb{Z}_6 / \\{0, 3\\}$

The cosets are $\\{0, 3\\}$, $\\{1, 4\\}$, $\\{2, 5\\}$. Labeling them $\\bar{0}, \\bar{1}, \\bar{2}$:

| + | $\\bar{0}$ | $\\bar{1}$ | $\\bar{2}$ |
|---|---|---|---|
| $\\bar{0}$ | $\\bar{0}$ | $\\bar{1}$ | $\\bar{2}$ |
| $\\bar{1}$ | $\\bar{1}$ | $\\bar{2}$ | $\\bar{0}$ |
| $\\bar{2}$ | $\\bar{2}$ | $\\bar{0}$ | $\\bar{1}$ |

This is isomorphic to $\\mathbb{Z}_3$!

### First Isomorphism Theorem

If $\\phi: G \\to H$ is a homomorphism, then:

$$G / \\ker(\\phi) \\cong \\text{im}(\\phi)$$

This is one of the most important theorems in algebra.

### Your Task

Implement \`quotient_cayley_table(n, subgroup)\` that computes the Cayley table for $\\mathbb{Z}_n / H$ where $H$ is a subgroup. Represent each coset by its smallest element. Return the table as a list of lists.`,

	starterCode: `def quotient_cayley_table(n, subgroup):
    # 1. Find all distinct cosets, label by smallest element
    # 2. Build Cayley table: adding representatives mod n,
    #    then find which coset the result belongs to
    pass

# Z_6 / {0, 3} should give Z_3
table = quotient_cayley_table(6, [0, 3])
for row in table:
    print(row)

# Z_8 / {0, 4} should give Z_4-like structure
table2 = quotient_cayley_table(8, [0, 4])
for row in table2:
    print(row)
`,

	solution: `def quotient_cayley_table(n, subgroup):
    sub_set = set(subgroup)
    # Find coset representatives (smallest element of each coset)
    seen = set()
    reps = []
    for a in range(n):
        coset = frozenset((a + h) % n for h in subgroup)
        if coset not in seen:
            seen.add(coset)
            reps.append(min(coset))
    # Map each element to its coset representative
    elem_to_rep = {}
    for rep in reps:
        coset = set((rep + h) % n for h in subgroup)
        for elem in coset:
            elem_to_rep[elem] = rep
    # Build Cayley table
    rep_to_idx = {rep: i for i, rep in enumerate(reps)}
    k = len(reps)
    table = []
    for i in range(k):
        row = []
        for j in range(k):
            result = (reps[i] + reps[j]) % n
            row.append(rep_to_idx[elem_to_rep[result]])
        table.append(row)
    return table

# Z_6 / {0, 3} should give Z_3
table = quotient_cayley_table(6, [0, 3])
for row in table:
    print(row)

# Z_8 / {0, 4} should give Z_4-like structure
table2 = quotient_cayley_table(8, [0, 4])
for row in table2:
    print(row)
`,

	tests: [
		{
			name: "Z_6/{0,3} is Z_3, Z_8/{0,4} is Z_4",
			expected:
				"[0, 1, 2]\n[1, 2, 0]\n[2, 0, 1]\n[0, 1, 2, 3]\n[1, 2, 3, 0]\n[2, 3, 0, 1]\n[3, 0, 1, 2]\n",
		},
		{
			name: "Z_4/{0,2} is Z_2",
			code: `{{FUNC}}
table = quotient_cayley_table(4, [0, 2])
for row in table:
    print(row)`,
			expected: "[0, 1]\n[1, 0]\n",
		},
		{
			name: "Z_6/{0} is Z_6 itself",
			code: `{{FUNC}}
table = quotient_cayley_table(6, [0])
print(len(table))`,
			expected: "6\n",
		},
	],
};
