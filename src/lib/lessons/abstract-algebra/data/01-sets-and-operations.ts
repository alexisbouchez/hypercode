import type { Lesson } from "../../types";

export const setsAndOperations: Lesson = {
	id: "sets-and-operations",
	title: "Sets & Binary Operations",
	chapterId: "groups",
	content: `## Sets & Binary Operations

Abstract algebra studies **algebraic structures** — sets equipped with operations that satisfy certain axioms. Before we dive into groups, we need to understand binary operations and their properties.

### Binary Operations

A **binary operation** $*$ on a set $S$ is a function $*: S \\times S \\to S$. The operation is **closed** if for all $a, b \\in S$, we have $a * b \\in S$.

For example, addition on the integers $\\mathbb{Z}$ is a binary operation: for any two integers $a$ and $b$, $a + b$ is also an integer.

### Modular Arithmetic as a Set

The set $\\mathbb{Z}_n = \\{0, 1, 2, \\ldots, n-1\\}$ with addition modulo $n$ is a fundamental example. We can represent this in Python:

\`\`\`python
def mod_add(a, b, n):
    return (a + b) % n

def mod_mul(a, b, n):
    return (a * b) % n
\`\`\`

### Properties of Binary Operations

A binary operation $*$ on $S$ may have these properties:

- **Associative**: $(a * b) * c = a * (b * c)$ for all $a, b, c \\in S$
- **Commutative**: $a * b = b * a$ for all $a, b \\in S$
- **Identity element**: There exists $e \\in S$ such that $e * a = a * e = a$ for all $a$
- **Inverse**: For each $a \\in S$, there exists $a^{-1} \\in S$ such that $a * a^{-1} = a^{-1} * a = e$

### Cayley Table

A **Cayley table** displays the results of a binary operation. For $\\mathbb{Z}_4$ under addition:

| + | 0 | 1 | 2 | 3 |
|---|---|---|---|---|
| 0 | 0 | 1 | 2 | 3 |
| 1 | 1 | 2 | 3 | 0 |
| 2 | 2 | 3 | 0 | 1 |
| 3 | 3 | 0 | 1 | 2 |

### Your Task

Implement \`cayley_table(n)\` that returns the Cayley table (as a list of lists) for addition modulo $n$, and \`is_commutative(table)\` that checks whether a Cayley table represents a commutative operation.`,

	starterCode: `def cayley_table(n):
    # Return an n x n list-of-lists for addition mod n
    pass

def is_commutative(table):
    # Check if table[i][j] == table[j][i] for all i, j
    pass

table = cayley_table(4)
for row in table:
    print(row)
print(is_commutative(table))
`,

	solution: `def cayley_table(n):
    return [[(i + j) % n for j in range(n)] for i in range(n)]

def is_commutative(table):
    n = len(table)
    for i in range(n):
        for j in range(n):
            if table[i][j] != table[j][i]:
                return False
    return True

table = cayley_table(4)
for row in table:
    print(row)
print(is_commutative(table))
`,

	tests: [
		{
			name: "Cayley table for Z_4 and commutativity",
			expected:
				"[0, 1, 2, 3]\n[1, 2, 3, 0]\n[2, 3, 0, 1]\n[3, 0, 1, 2]\nTrue\n",
		},
		{
			name: "Cayley table for Z_3",
			code: `{{FUNC}}
table = cayley_table(3)
for row in table:
    print(row)`,
			expected: "[0, 1, 2]\n[1, 2, 0]\n[2, 0, 1]\n",
		},
		{
			name: "Non-commutative table detection",
			code: `{{FUNC}}
table = [[0, 1], [2, 0]]
print(is_commutative(table))`,
			expected: "False\n",
		},
	],
};
