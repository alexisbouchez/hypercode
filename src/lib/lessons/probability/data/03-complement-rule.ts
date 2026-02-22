import type { Lesson } from "../../types";

export const complementRule: Lesson = {
	id: "complement-rule",
	title: "Complement Rule & At-Least-One Problems",
	chapterId: "foundations",
	content: `## The Complement Rule

From the axioms: $P(A) + P(A^c) = 1$, so:

$$P(A^c) = 1 - P(A)$$

This is more powerful than it looks. Many problems are easier to solve via the complement.

### At-Least-One Problems

*What is the probability of getting at least one success in $n$ independent trials?*

The complement is "zero successes in all $n$ trials":

$$P(\\text{at least one}) = 1 - P(\\text{all failures}) = 1 - (1-p)^n$$

\`\`\`python
# P(at least one head in 3 fair coin flips)
p_failure = 0.5   # P(tails on one flip)
n = 3
p = 1 - p_failure**n
print(round(p, 4))  # 0.875
\`\`\`

### Birthday Problem

How many people do you need so there's a >50% chance two share a birthday? The complement is "all birthdays distinct":

$$P(\\text{match}) = 1 - \\frac{365 \\cdot 364 \\cdots (365-n+1)}{365^n}$$

The answer is just **23 people** — a famously counterintuitive result.

### Your Task

Implement \`at_least_one(p_failure, n_trials)\` that returns $P(\\text{at least one success}) = 1 - p_{\\text{failure}}^n$, rounded to 4 decimal places.`,

	starterCode: `def at_least_one(p_failure, n_trials):
    # P(at least one success) = 1 - p_failure^n_trials, rounded to 4 decimal places
    return 0.0

print(at_least_one(0.5, 2))  # 0.75 — at least one head in 2 flips
`,

	solution: `def at_least_one(p_failure, n_trials):
    return round(1 - p_failure**n_trials, 4)

print(at_least_one(0.5, 2))
`,

	tests: [
		{
			name: "at_least_one(0.5, 2) = 0.75",
			expected: "0.75\n",
		},
		{
			name: "at_least_one(0.5, 1) = 0.5",
			code: `{{FUNC}}
print(at_least_one(0.5, 1))`,
			expected: "0.5\n",
		},
		{
			name: "at_least_one(0.9, 5) = 0.4095",
			code: `{{FUNC}}
print(at_least_one(0.9, 5))`,
			expected: "0.4095\n",
		},
		{
			name: "at_least_one(0.5, 10) = 0.999",
			code: `{{FUNC}}
print(at_least_one(0.5, 10))`,
			expected: "0.999\n",
		},
	],
};
