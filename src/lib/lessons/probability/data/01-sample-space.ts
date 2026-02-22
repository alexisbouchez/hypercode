import type { Lesson } from "../../types";

export const sampleSpace: Lesson = {
	id: "sample-space",
	title: "Sample Spaces & Events",
	chapterId: "foundations",
	content: `## The Language of Chance

A **sample space** $\\Omega$ is the set of all possible outcomes of a random experiment. An **event** is any subset of $\\Omega$.

**Classical probability** applies when all outcomes are equally likely:

$$P(A) = \\frac{|A|}{|\\Omega|}$$

where $|A|$ is the number of outcomes in event $A$.

\`\`\`python
# Rolling a fair die
omega = [1, 2, 3, 4, 5, 6]
even  = [2, 4, 6]

p = len(even) / len(omega)
print(round(p, 4))  # 0.5
\`\`\`

### Set Operations on Events

| Operation | Notation | Meaning |
|---|---|---|
| Union | $A \\cup B$ | A or B occurs |
| Intersection | $A \\cap B$ | A and B both occur |
| Complement | $A^c$ | A does not occur |

### Your Task

Implement \`classical_probability(outcomes, event)\` that returns $P(A) = |\\text{event}| / |\\text{outcomes}|$, rounded to 4 decimal places.`,

	starterCode: `def classical_probability(outcomes, event):
    # P(A) = |event| / |outcomes|, rounded to 4 decimal places
    return 0.0

die = [1, 2, 3, 4, 5, 6]
even = [2, 4, 6]
print(classical_probability(die, even))  # 0.5
`,

	solution: `def classical_probability(outcomes, event):
    return round(len(event) / len(outcomes), 4)

die = [1, 2, 3, 4, 5, 6]
even = [2, 4, 6]
print(classical_probability(die, even))
`,

	tests: [
		{
			name: "P(even) on a die = 0.5",
			expected: "0.5\n",
		},
		{
			name: "P(rolling 1) = 0.1667",
			code: `{{FUNC}}
print(classical_probability([1,2,3,4,5,6], [1]))`,
			expected: "0.1667\n",
		},
		{
			name: "P(heads) on a fair coin = 0.5",
			code: `{{FUNC}}
print(classical_probability(['H','T'], ['H']))`,
			expected: "0.5\n",
		},
		{
			name: "P(even) in {1..10} = 0.5",
			code: `{{FUNC}}
print(classical_probability(list(range(1,11)), [2,4,6,8,10]))`,
			expected: "0.5\n",
		},
	],
};
