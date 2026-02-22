import type { Lesson } from "../../types";

export const propositionalLogic: Lesson = {
	id: "propositional-logic",
	title: "Propositional Logic",
	chapterId: "logic-proofs",
	content: `## The Algebra of True and False

**Propositional logic** is the foundation of mathematical reasoning. A **proposition** is any statement that is either true or false. We combine propositions using **logical connectives**:

| Symbol | Name | Meaning |
|--------|------|---------|
| $\\neg p$ | Negation | not p |
| $p \\land q$ | Conjunction | p and q |
| $p \\lor q$ | Disjunction | p or q |
| $p \\Rightarrow q$ | Implication | if p then q |
| $p \\Leftrightarrow q$ | Biconditional | p iff q |

A **tautology** is a formula that is true for every assignment of truth values. A **contradiction** is always false. A **satisfiable** formula is true for at least one assignment.

**De Morgan's Laws** are fundamental:
$$\\neg(p \\land q) \\equiv \\neg p \\lor \\neg q$$
$$\\neg(p \\lor q) \\equiv \\neg p \\land \\neg q$$

\`\`\`python
# Check De Morgan's first law
def check_tautology(expr):
    for p in [True, False]:
        for q in [True, False]:
            if not expr(p, q):
                return False
    return True

demorgan = lambda p, q: (not (p and q)) == (not p or not q)
print(check_tautology(demorgan))  # True
\`\`\`

### Your Task

Implement:
- \`check_tautology(expr)\` — returns \`True\` if the 2-variable boolean function is a tautology
- \`count_satisfying(expr, n_vars)\` — returns how many truth assignments satisfy \`expr\``,

	starterCode: `from itertools import product

def check_tautology(expr):
    # Try all combinations of p, q in {True, False}
    # Return True if expr(p, q) is True for all of them
    pass

def count_satisfying(expr, n_vars):
    # Count how many truth assignments satisfy expr
    # Use itertools.product([True, False], repeat=n_vars)
    pass

# De Morgan's law is a tautology
demorgan = lambda p, q: (not (p and q)) == (not p or not q)
print(check_tautology(demorgan))  # True
`,

	solution: `from itertools import product

def check_tautology(expr):
    for vals in product([True, False], repeat=2):
        if not expr(*vals):
            return False
    return True

def count_satisfying(expr, n_vars):
    count = 0
    for vals in product([True, False], repeat=n_vars):
        if expr(*vals):
            count += 1
    return count

demorgan = lambda p, q: (not (p and q)) == (not p or not q)
print(check_tautology(demorgan))
`,

	tests: [
		{
			name: "De Morgan's law is a tautology",
			expected: "True\n",
		},
		{
			name: "p OR NOT p is a tautology",
			code: `{{FUNC}}
print(check_tautology(lambda p, q: p or not p))`,
			expected: "True\n",
		},
		{
			name: "p OR q satisfied by 3 of 4 assignments",
			code: `{{FUNC}}
print(count_satisfying(lambda p, q: p or q, 2))`,
			expected: "3\n",
		},
		{
			name: "p AND NOT p is not a tautology",
			code: `{{FUNC}}
print(check_tautology(lambda p, q: p and not p))`,
			expected: "False\n",
		},
	],
};
