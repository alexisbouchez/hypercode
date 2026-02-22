import type { Lesson } from "../../types";

export const relations: Lesson = {
	id: "relations",
	title: "Relations",
	chapterId: "sets-relations-functions",
	content: `## Binary Relations

A **binary relation** $R$ on a set $A$ is a subset of $A \\times A$. We write $aRb$ (or $(a,b) \\in R$) when $a$ is related to $b$.

### Key Properties

| Property | Definition | Example |
|----------|------------|---------|
| **Reflexive** | $\\forall a,\\ (a,a) \\in R$ | $\\leq$ on $\\mathbb{Z}$ |
| **Symmetric** | $(a,b) \\in R \\Rightarrow (b,a) \\in R$ | $=$ on $\\mathbb{Z}$ |
| **Antisymmetric** | $(a,b),(b,a) \\in R \\Rightarrow a=b$ | $\\leq$ on $\\mathbb{Z}$ |
| **Transitive** | $(a,b),(b,c) \\in R \\Rightarrow (a,c) \\in R$ | $<$ on $\\mathbb{Z}$ |

### Equivalence Relations

A relation that is reflexive, symmetric, and transitive is an **equivalence relation**. It partitions the domain into disjoint **equivalence classes** $[a] = \\{b : aRb\\}$.

**Example**: "same remainder mod 3" is an equivalence relation on $\\mathbb{Z}$, with classes $[0], [1], [2]$.

\`\`\`python
def is_equivalence(relation, domain):
    return (is_reflexive(relation, domain) and
            is_symmetric(relation) and
            is_transitive(relation))

# R = {(1,1),(2,2),(3,3),(1,2),(2,1)} — two classes: {1,2} and {3}
R = {(1,1),(2,2),(3,3),(1,2),(2,1)}
print(is_equivalence(R, {1,2,3}))  # True
\`\`\`

### Your Task

Implement \`is_reflexive\`, \`is_symmetric\`, \`is_transitive\`, and \`is_equivalence\`.`,

	starterCode: `def is_reflexive(relation, domain):
    # (x, x) must be in relation for every x in domain
    pass

def is_symmetric(relation):
    # If (a, b) is in relation, then (b, a) must also be
    pass

def is_transitive(relation):
    # If (a, b) and (b, c) are in relation, then (a, c) must be too
    pass

def is_equivalence(relation, domain):
    return (is_reflexive(relation, domain) and
            is_symmetric(relation) and
            is_transitive(relation))

R = {(1,1),(2,2),(3,3),(1,2),(2,1)}
print(is_equivalence(R, {1,2,3}))  # True
`,

	solution: `def is_reflexive(relation, domain):
    return all((x, x) in relation for x in domain)

def is_symmetric(relation):
    return all((b, a) in relation for (a, b) in relation)

def is_transitive(relation):
    for (a, b) in relation:
        for (c, d) in relation:
            if b == c and (a, d) not in relation:
                return False
    return True

def is_equivalence(relation, domain):
    return (is_reflexive(relation, domain) and
            is_symmetric(relation) and
            is_transitive(relation))

R = {(1,1),(2,2),(3,3),(1,2),(2,1)}
print(is_equivalence(R, {1,2,3}))
`,

	tests: [
		{
			name: "is_equivalence: {(1,1),(2,2),(3,3),(1,2),(2,1)} on {1,2,3}",
			expected: "True\n",
		},
		{
			name: "is_reflexive: {(1,1),(2,2),(3,3)} on {1,2,3}",
			code: `{{FUNC}}
print(is_reflexive({(1,1),(2,2),(3,3)}, {1,2,3}))`,
			expected: "True\n",
		},
		{
			name: "is_symmetric: {(1,2),(2,1),(3,3)}",
			code: `{{FUNC}}
print(is_symmetric({(1,2),(2,1),(3,3)}))`,
			expected: "True\n",
		},
		{
			name: "is_transitive: {(1,2),(2,3),(1,3)}",
			code: `{{FUNC}}
print(is_transitive({(1,2),(2,3),(1,3)}))`,
			expected: "True\n",
		},
	],
};
