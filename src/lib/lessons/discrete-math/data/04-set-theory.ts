import type { Lesson } from "../../types";

export const setTheory: Lesson = {
	id: "set-theory",
	title: "Set Theory",
	chapterId: "sets-relations-functions",
	content: `## Sets: The Foundation of Mathematics

A **set** is an unordered collection of distinct objects. Sets are described by **membership**: $x \\in A$ means $x$ belongs to $A$.

### Key Operations

| Operation | Notation | Definition |
|-----------|----------|------------|
| Union | $A \\cup B$ | $\\{x : x \\in A \\text{ or } x \\in B\\}$ |
| Intersection | $A \\cap B$ | $\\{x : x \\in A \\text{ and } x \\in B\\}$ |
| Difference | $A \\setminus B$ | $\\{x : x \\in A \\text{ and } x \\notin B\\}$ |
| Complement | $A^c$ | $\\{x \\in U : x \\notin A\\}$ |
| Power set | $\\mathcal{P}(A)$ | Set of all subsets of $A$ |
| Cartesian product | $A \\times B$ | $\\{(a,b) : a \\in A, b \\in B\\}$ |

### Power Set

If $|A| = n$, then $|\\mathcal{P}(A)| = 2^n$. Every element either is or isn't in a subset — $n$ binary choices.

\`\`\`python
def power_set(s):
    result = [frozenset()]
    for elem in sorted(s):
        result = result + [subset | {elem} for subset in result]
    return result

print(len(power_set({1, 2, 3})))  # 8 = 2^3
\`\`\`

### Cartesian Product

$|A \\times B| = |A| \\cdot |B|$ — the product rule for counting.

\`\`\`python
def cartesian_product(A, B):
    return [(a, b) for a in sorted(A) for b in sorted(B)]

print(len(cartesian_product({1, 2}, {3, 4, 5})))  # 6 = 2 × 3
\`\`\`

### Your Task

Implement \`power_set(s)\` and \`cartesian_product(A, B)\` as shown above.`,

	starterCode: `def power_set(s):
    # Start with {∅}, then for each element add it to every existing subset
    result = [frozenset()]
    for elem in sorted(s):
        result = result + [subset | {elem} for subset in result]
    return result

def cartesian_product(A, B):
    # Return list of all (a, b) pairs
    pass

print(len(power_set({1, 2, 3})))  # 8
print(len(cartesian_product({1, 2}, {3, 4, 5})))  # 6
`,

	solution: `def power_set(s):
    result = [frozenset()]
    for elem in sorted(s):
        result = result + [subset | {elem} for subset in result]
    return result

def cartesian_product(A, B):
    return [(a, b) for a in sorted(A) for b in sorted(B)]

print(len(power_set({1, 2, 3})))
print(len(cartesian_product({1, 2}, {3, 4, 5})))
`,

	tests: [
		{
			name: "|P({1,2,3})| = 8",
			code: `{{FUNC}}
print(len(power_set({1, 2, 3})))`,
			expected: "8\n",
		},
		{
			name: "|{1,2} × {3,4,5}| = 6",
			code: `{{FUNC}}
print(len(cartesian_product({1, 2}, {3, 4, 5})))`,
			expected: "6\n",
		},
		{
			name: "|P(∅)| = 1",
			code: `{{FUNC}}
print(len(power_set(set())))`,
			expected: "1\n",
		},
		{
			name: "|P({1,2,3,4})| = 16",
			code: `{{FUNC}}
print(len(power_set({1, 2, 3, 4})))`,
			expected: "16\n",
		},
	],
};
