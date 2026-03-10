import type { Lesson } from "../../types";

export const groups: Lesson = {
	id: "groups",
	title: "Groups",
	chapterId: "groups",
	content: `## Groups

A **group** $(G, *)$ is a set $G$ with a binary operation $*$ satisfying four axioms:

1. **Closure**: For all $a, b \\in G$, $a * b \\in G$
2. **Associativity**: $(a * b) * c = a * (b * c)$
3. **Identity**: There exists $e \\in G$ such that $e * a = a * e = a$ for all $a$
4. **Inverse**: For each $a \\in G$, there exists $a^{-1}$ such that $a * a^{-1} = a^{-1} * a = e$

### Example: $\\mathbb{Z}_n$ under Addition

The set $\\{0, 1, \\ldots, n-1\\}$ with addition modulo $n$ forms a group:
- Identity: $0$
- Inverse of $a$: $n - a$ (mod $n$)

\`\`\`python
def identity(n):
    return 0

def inverse(a, n):
    return (n - a) % n
\`\`\`

### Verifying Group Axioms

We can check whether a given Cayley table satisfies the group axioms computationally:

\`\`\`python
def has_identity(table):
    n = len(table)
    for e in range(n):
        if all(table[e][a] == a and table[a][e] == a for a in range(n)):
            return e
    return -1
\`\`\`

### Order of a Group

The **order** of a group is the number of elements: $|G|$. The group $\\mathbb{Z}_n$ has order $n$.

### Your Task

Implement \`verify_group(table)\` that takes a Cayley table (list of lists using elements $\\{0, 1, \\ldots, n-1\\}$) and returns \`True\` if it defines a group. Check closure (all entries in range), associativity, identity, and inverses.`,

	starterCode: `def verify_group(table):
    n = len(table)
    # Check closure: all entries in {0, ..., n-1}
    # Find identity element
    # Check associativity
    # Check inverses
    pass

# Z_4 addition table (should be a group)
z4 = [[(i + j) % 4 for j in range(4)] for i in range(4)]
print(verify_group(z4))

# Not a group (no identity)
bad = [[0, 0], [0, 0]]
print(verify_group(bad))
`,

	solution: `def verify_group(table):
    n = len(table)
    # Check closure
    for i in range(n):
        for j in range(n):
            if table[i][j] < 0 or table[i][j] >= n:
                return False
    # Find identity
    identity = -1
    for e in range(n):
        if all(table[e][a] == a and table[a][e] == a for a in range(n)):
            identity = e
            break
    if identity == -1:
        return False
    # Check associativity
    for a in range(n):
        for b in range(n):
            for c in range(n):
                if table[table[a][b]][c] != table[a][table[b][c]]:
                    return False
    # Check inverses
    for a in range(n):
        found = False
        for b in range(n):
            if table[a][b] == identity and table[b][a] == identity:
                found = True
                break
        if not found:
            return False
    return True

# Z_4 addition table (should be a group)
z4 = [[(i + j) % 4 for j in range(4)] for i in range(4)]
print(verify_group(z4))

# Not a group (no identity)
bad = [[0, 0], [0, 0]]
print(verify_group(bad))
`,

	tests: [
		{
			name: "Z_4 is a group, bad table is not",
			expected: "True\nFalse\n",
		},
		{
			name: "Z_3 addition is a group",
			code: `{{FUNC}}
z3 = [[(i + j) % 3 for j in range(3)] for i in range(3)]
print(verify_group(z3))`,
			expected: "True\n",
		},
		{
			name: "Z_5 addition is a group",
			code: `{{FUNC}}
z5 = [[(i + j) % 5 for j in range(5)] for i in range(5)]
print(verify_group(z5))`,
			expected: "True\n",
		},
		{
			name: "Non-associative table is not a group",
			code: `{{FUNC}}
# Identity is 0, but not associative
t = [[0,1,2],[1,2,0],[2,0,1]]
t[1][2] = 1
print(verify_group(t))`,
			expected: "False\n",
		},
	],
};
