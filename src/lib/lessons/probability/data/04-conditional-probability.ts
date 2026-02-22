import type { Lesson } from "../../types";

export const conditionalProbability: Lesson = {
	id: "conditional-probability",
	title: "Conditional Probability",
	chapterId: "conditional",
	content: `## Updating on Evidence

**Conditional probability** $P(A \\mid B)$ is the probability of $A$ given that $B$ has occurred:

$$P(A \\mid B) = \\frac{P(A \\cap B)}{P(B)}$$

Conditioning on $B$ *reduces* the sample space to only outcomes where $B$ holds.

\`\`\`python
# Roll two dice. Given the sum > 7, what is P(first die = 6)?
# Outcomes where sum > 7: (2,6),(3,5),(3,6),(4,4),(4,5),(4,6),(5,3)...(6,6) → 15 outcomes
# Of these, first die = 6: (6,2),(6,3),(6,4),(6,5),(6,6) → 5 outcomes
# P(first=6 | sum>7) = 5/15 = 1/3

p_ab = 5/36   # P(first=6 AND sum>7)
p_b  = 15/36  # P(sum > 7)
print(round(p_ab / p_b, 4))  # 0.3333
\`\`\`

### Chain Rule

$$P(A \\cap B) = P(A \\mid B) \\cdot P(B) = P(B \\mid A) \\cdot P(A)$$

This extends to any number of events:
$$P(A_1 \\cap A_2 \\cap \\cdots \\cap A_n) = P(A_1) \\cdot P(A_2 \\mid A_1) \\cdot P(A_3 \\mid A_1, A_2) \\cdots$$

### Your Task

Implement \`conditional(p_ab, p_b)\` that returns $P(A \\mid B) = P(A \\cap B) / P(B)$, rounded to 4 decimal places.`,

	starterCode: `def conditional(p_ab, p_b):
    # P(A|B) = P(A∩B) / P(B), rounded to 4 decimal places
    return 0.0

print(conditional(0.3, 0.6))  # 0.5
`,

	solution: `def conditional(p_ab, p_b):
    return round(p_ab / p_b, 4)

print(conditional(0.3, 0.6))
`,

	tests: [
		{
			name: "conditional(0.3, 0.6) = 0.5",
			expected: "0.5\n",
		},
		{
			name: "conditional(0.1, 0.4) = 0.25",
			code: `{{FUNC}}
print(conditional(0.1, 0.4))`,
			expected: "0.25\n",
		},
		{
			name: "conditioning on certain event: conditional(0.5, 1.0) = 0.5",
			code: `{{FUNC}}
print(conditional(0.5, 1.0))`,
			expected: "0.5\n",
		},
		{
			name: "conditional(0.25, 0.5) = 0.5",
			code: `{{FUNC}}
print(conditional(0.25, 0.5))`,
			expected: "0.5\n",
		},
	],
};
