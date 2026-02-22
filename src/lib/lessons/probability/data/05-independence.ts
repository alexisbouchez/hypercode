import type { Lesson } from "../../types";

export const independence: Lesson = {
	id: "independence",
	title: "Independence",
	chapterId: "conditional",
	content: `## When Events Don't Influence Each Other

Events $A$ and $B$ are **independent** if knowing $B$ occurred tells you nothing about $A$:

$$P(A \\mid B) = P(A)$$

Equivalently (and more useful in practice):

$$P(A \\cap B) = P(A) \\cdot P(B)$$

\`\`\`python
# Two fair coin flips are independent
p_heads_1 = 0.5
p_heads_2 = 0.5
p_both = p_heads_1 * p_heads_2

print(p_both)  # 0.25 — P(HH)
\`\`\`

### Testing for Independence

Given $P(A)$, $P(B)$, and $P(A \\cap B)$, events are independent iff:

$$|P(A \\cap B) - P(A) \\cdot P(B)| < \\epsilon$$

### Mutual Exclusivity ≠ Independence

Mutually exclusive events (where $P(A \\cap B) = 0$) are almost never independent (unless one has probability 0). If $A$ occurs, $B$ definitely cannot — that is dependence.

### Your Task

Implement two functions:
1. \`are_independent(p_a, p_b, p_ab)\` — returns \`True\` if $|P(A \\cap B) - P(A) \\cdot P(B)| < 10^{-9}$
2. \`joint_independent(p_a, p_b)\` — returns $P(A) \\cdot P(B)$, rounded to 4 decimal places

Print the independence check, then the joint probability.`,

	starterCode: `def are_independent(p_a, p_b, p_ab):
    # True if |p_ab - p_a * p_b| < 1e-9
    return False

def joint_independent(p_a, p_b):
    # P(A) * P(B), rounded to 4 decimal places
    return 0.0

p_a, p_b, p_ab = 0.5, 0.5, 0.25
print(are_independent(p_a, p_b, p_ab))
print(joint_independent(p_a, p_b))
`,

	solution: `def are_independent(p_a, p_b, p_ab):
    return abs(p_ab - p_a * p_b) < 1e-9

def joint_independent(p_a, p_b):
    return round(p_a * p_b, 4)

p_a, p_b, p_ab = 0.5, 0.5, 0.25
print(are_independent(p_a, p_b, p_ab))
print(joint_independent(p_a, p_b))
`,

	tests: [
		{
			name: "two fair coins: independent, joint = 0.25",
			expected: "True\n0.25\n",
		},
		{
			name: "independent: p_a=0.3, p_b=0.4, p_ab=0.12",
			code: `{{FUNC}}
print(are_independent(0.3, 0.4, 0.12))
print(joint_independent(0.3, 0.4))`,
			expected: "True\n0.12\n",
		},
		{
			name: "A=B, not independent: p_a=0.5, p_b=0.5, p_ab=0.5",
			code: `{{FUNC}}
print(are_independent(0.5, 0.5, 0.5))
print(joint_independent(0.5, 0.5))`,
			expected: "False\n0.25\n",
		},
		{
			name: "not independent: p_a=0.6, p_b=0.5, p_ab=0.2",
			code: `{{FUNC}}
print(are_independent(0.6, 0.5, 0.2))
print(joint_independent(0.6, 0.5))`,
			expected: "False\n0.3\n",
		},
	],
};
