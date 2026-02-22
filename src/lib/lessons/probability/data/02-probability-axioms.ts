import type { Lesson } from "../../types";

export const probabilityAxioms: Lesson = {
	id: "probability-axioms",
	title: "Probability Axioms & Addition Rule",
	chapterId: "foundations",
	content: `## Kolmogorov's Axioms

Every probability measure $P$ must satisfy three axioms:

1. **Non-negativity**: $P(A) \\geq 0$ for all events $A$
2. **Normalization**: $P(\\Omega) = 1$
3. **Countable additivity**: if $A \\cap B = \\emptyset$, then $P(A \\cup B) = P(A) + P(B)$

These three axioms are all you need to derive all of probability theory.

### The Addition Rule

For any two events (not necessarily disjoint):

$$P(A \\cup B) = P(A) + P(B) - P(A \\cap B)$$

The $P(A \\cap B)$ term corrects for double-counting outcomes in both $A$ and $B$.

\`\`\`python
# Drawing a card: P(heart or face card)
p_heart = 13/52       # P(A)
p_face  = 12/52       # P(B)
p_both  = 3/52        # P(A ∩ B): face cards that are hearts

p_union = p_heart + p_face - p_both
print(round(p_union, 4))  # 0.4231
\`\`\`

**Mutually exclusive** events ($A \\cap B = \\emptyset$) simplify to $P(A \\cup B) = P(A) + P(B)$.

### Your Task

Implement \`union_probability(p_a, p_b, p_ab)\` that returns $P(A \\cup B)$, rounded to 4 decimal places.`,

	starterCode: `def union_probability(p_a, p_b, p_ab):
    # P(A ∪ B) = P(A) + P(B) - P(A ∩ B), rounded to 4 decimal places
    return 0.0

print(union_probability(0.3, 0.4, 0.1))  # 0.6
`,

	solution: `def union_probability(p_a, p_b, p_ab):
    return round(p_a + p_b - p_ab, 4)

print(union_probability(0.3, 0.4, 0.1))
`,

	tests: [
		{
			name: "union_probability(0.3, 0.4, 0.1) = 0.6",
			expected: "0.6\n",
		},
		{
			name: "mutually exclusive: union_probability(0.5, 0.5, 0.0) = 1.0",
			code: `{{FUNC}}
print(union_probability(0.5, 0.5, 0.0))`,
			expected: "1.0\n",
		},
		{
			name: "disjoint events: union_probability(0.3, 0.4, 0.0) = 0.7",
			code: `{{FUNC}}
print(union_probability(0.3, 0.4, 0.0))`,
			expected: "0.7\n",
		},
		{
			name: "union_probability(0.6, 0.5, 0.3) = 0.8",
			code: `{{FUNC}}
print(union_probability(0.6, 0.5, 0.3))`,
			expected: "0.8\n",
		},
	],
};
