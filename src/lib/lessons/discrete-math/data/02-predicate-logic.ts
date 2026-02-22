import type { Lesson } from "../../types";

export const predicateLogic: Lesson = {
	id: "predicate-logic",
	title: "Predicate Logic & Quantifiers",
	chapterId: "logic-proofs",
	content: `## From Propositions to Predicates

**Predicate logic** (first-order logic) extends propositional logic with **predicates** — statements about objects — and **quantifiers** that range over domains.

A **predicate** $P(x)$ is a statement about $x$ that becomes a proposition when $x$ is given a value. The two quantifiers are:

$$\\forall x \\in D,\\ P(x) \\quad \\text{(universal: P holds for every x in D)}$$
$$\\exists x \\in D,\\ P(x) \\quad \\text{(existential: P holds for at least one x in D)}$$

**Negation of quantifiers** (De Morgan for quantifiers):
$$\\neg(\\forall x,\\ P(x)) \\equiv \\exists x,\\ \\neg P(x)$$
$$\\neg(\\exists x,\\ P(x)) \\equiv \\forall x,\\ \\neg P(x)$$

\`\`\`python
def for_all(domain, predicate):
    return all(predicate(x) for x in domain)

def exists(domain, predicate):
    return any(predicate(x) for x in domain)

# Every number in {1..5} is positive
print(for_all(range(1, 6), lambda x: x > 0))  # True
# Some number in {1..5} equals 3
print(exists(range(1, 6), lambda x: x == 3))  # True
\`\`\`

### Nested Quantifiers

Order matters: $\\forall x\\, \\exists y,\\ P(x,y)$ differs from $\\exists y\\, \\forall x,\\ P(x,y)$.

### Your Task

Implement \`for_all(domain, predicate)\` and \`exists(domain, predicate)\` using Python's built-in \`all\` and \`any\`.`,

	starterCode: `def for_all(domain, predicate):
    # Return True if predicate(x) is True for every x in domain
    pass

def exists(domain, predicate):
    # Return True if predicate(x) is True for at least one x in domain
    pass

print(for_all(range(1, 6), lambda x: x > 0))  # True
print(exists(range(1, 6), lambda x: x == 3))  # True
`,

	solution: `def for_all(domain, predicate):
    return all(predicate(x) for x in domain)

def exists(domain, predicate):
    return any(predicate(x) for x in domain)

print(for_all(range(1, 6), lambda x: x > 0))
print(exists(range(1, 6), lambda x: x == 3))
`,

	tests: [
		{
			name: "for_all: every element of {1..5} is positive",
			code: `{{FUNC}}
print(for_all(range(1, 6), lambda x: x > 0))`,
			expected: "True\n",
		},
		{
			name: "exists: some element of {1..5} equals 3",
			code: `{{FUNC}}
print(exists(range(1, 6), lambda x: x == 3))`,
			expected: "True\n",
		},
		{
			name: "for_all: NOT every element of {1..5} is even",
			code: `{{FUNC}}
print(for_all(range(1, 6), lambda x: x % 2 == 0))`,
			expected: "False\n",
		},
		{
			name: "exists: no element of {1..5} exceeds 10",
			code: `{{FUNC}}
print(exists(range(1, 6), lambda x: x > 10))`,
			expected: "False\n",
		},
	],
};
