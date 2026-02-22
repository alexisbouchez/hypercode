import type { Lesson } from "../../types";

export const bayesTheorem: Lesson = {
	id: "bayes-theorem",
	title: "Bayes' Theorem",
	chapterId: "conditional",
	content: `## Inverting Conditional Probabilities

**Bayes' theorem** lets you flip a conditional probability — turning $P(E \\mid H)$ into $P(H \\mid E)$:

$$P(H \\mid E) = \\frac{P(E \\mid H) \\cdot P(H)}{P(E)}$$

The denominator $P(E)$ comes from the **law of total probability**:

$$P(E) = P(E \\mid H) \\cdot P(H) + P(E \\mid H^c) \\cdot P(H^c)$$

### The Medical Test Example

A disease affects 1% of the population. A test has 99% sensitivity (true positive rate) and 5% false positive rate.

You test positive. What is the probability you have the disease?

$$P(H) = 0.01, \\quad P(E \\mid H) = 0.99, \\quad P(E \\mid H^c) = 0.05$$

$$P(E) = 0.99 \\times 0.01 + 0.05 \\times 0.99 = 0.0594$$

$$P(H \\mid E) = \\frac{0.99 \\times 0.01}{0.0594} \\approx 0.1667$$

Only **17%** — the low base rate dominates. This is the base-rate fallacy.

### Terminology

- $P(H)$ — **prior**: your belief before seeing evidence
- $P(H \\mid E)$ — **posterior**: updated belief after evidence
- $P(E \\mid H) / P(E)$ — **likelihood ratio**: how much the evidence updates you

### Your Task

Implement \`bayes(p_h, p_e_given_h, p_e_given_not_h)\` that returns $P(H \\mid E)$, rounded to 4 decimal places.`,

	starterCode: `def bayes(p_h, p_e_given_h, p_e_given_not_h):
    # P(H|E) = P(E|H)*P(H) / P(E)
    # P(E) = P(E|H)*P(H) + P(E|H^c)*(1-P(H))
    return 0.0

print(bayes(0.01, 0.99, 0.05))  # 0.1667
`,

	solution: `def bayes(p_h, p_e_given_h, p_e_given_not_h):
    p_e = p_e_given_h * p_h + p_e_given_not_h * (1 - p_h)
    return round(p_e_given_h * p_h / p_e, 4)

print(bayes(0.01, 0.99, 0.05))
`,

	tests: [
		{
			name: "medical test (1% base rate) = 0.1667",
			expected: "0.1667\n",
		},
		{
			name: "fair prior, strong evidence: bayes(0.5, 0.8, 0.2) = 0.8",
			code: `{{FUNC}}
print(bayes(0.5, 0.8, 0.2))`,
			expected: "0.8\n",
		},
		{
			name: "rare disease (0.1% base rate) = 0.0902",
			code: `{{FUNC}}
print(bayes(0.001, 0.99, 0.01))`,
			expected: "0.0902\n",
		},
		{
			name: "uninformative evidence: bayes(0.5, 0.5, 0.5) = 0.5",
			code: `{{FUNC}}
print(bayes(0.5, 0.5, 0.5))`,
			expected: "0.5\n",
		},
	],
};
