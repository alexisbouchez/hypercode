import type { Lesson } from "../../types";

export const sequences: Lesson = {
	id: "sequences",
	title: "Sequences",
	chapterId: "sequences",
	content: `## Sequences

A **sequence** is an ordered list of numbers indexed by the natural numbers. In real analysis, we write a sequence as \\((a_n)_{n=1}^{\\infty}\\) or simply \\((a_n)\\).

### Defining Sequences

A sequence is defined by a rule that assigns a real number to each natural number \\(n\\):

\`\`\`python
def a(n):
    return 1 / n  # The sequence 1, 1/2, 1/3, 1/4, ...
\`\`\`

### Common Sequences

| Sequence | Formula | First terms |
|----------|---------|-------------|
| Harmonic | \\(1/n\\) | 1, 1/2, 1/3, 1/4, ... |
| Geometric | \\(r^n\\) | r, r², r³, ... |
| Alternating | \\((-1)^n / n\\) | -1, 1/2, -1/3, ... |

### Computing Terms

We can compute and examine any finite number of terms:

\`\`\`python
terms = [1/n for n in range(1, 11)]
print(terms)
\`\`\`

### Bounded Sequences

A sequence is **bounded above** if there exists \\(M\\) such that \\(a_n \\le M\\) for all \\(n\\). It is **bounded below** if there exists \\(m\\) such that \\(a_n \\ge m\\) for all \\(n\\).

### Your Task

Implement \`compute_sequence(f, n)\` that takes a function \`f\` defining a sequence and an integer \`n\`, and returns a list of the first \`n\` terms: \`[f(1), f(2), ..., f(n)]\`.

Also implement \`is_bounded(f, n, lower, upper)\` that checks whether all of the first \`n\` terms satisfy \`lower <= f(k) <= upper\`.`,

	starterCode: `def compute_sequence(f, n):
    # Return [f(1), f(2), ..., f(n)]
    pass

def is_bounded(f, n, lower, upper):
    # Return True if lower <= f(k) <= upper for k = 1..n
    pass

harmonic = lambda n: 1 / n
geometric = lambda n: (1/2) ** n

print([round(x, 4) for x in compute_sequence(harmonic, 5)])
print([round(x, 4) for x in compute_sequence(geometric, 5)])
print(is_bounded(harmonic, 100, 0, 1))
`,

	solution: `def compute_sequence(f, n):
    return [f(k) for k in range(1, n + 1)]

def is_bounded(f, n, lower, upper):
    return all(lower <= f(k) <= upper for k in range(1, n + 1))

harmonic = lambda n: 1 / n
geometric = lambda n: (1/2) ** n

print([round(x, 4) for x in compute_sequence(harmonic, 5)])
print([round(x, 4) for x in compute_sequence(geometric, 5)])
print(is_bounded(harmonic, 100, 0, 1))
`,

	tests: [
		{
			name: "harmonic sequence first 5 terms",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print([round(x, 4) for x in compute_sequence(harmonic, 5)])`,
			expected: "[1.0, 0.5, 0.3333, 0.25, 0.2]\n",
		},
		{
			name: "geometric sequence first 5 terms",
			code: `{{FUNC}}
geometric = lambda n: (1/2) ** n
print([round(x, 4) for x in compute_sequence(geometric, 5)])`,
			expected: "[0.5, 0.25, 0.125, 0.0625, 0.0312]\n",
		},
		{
			name: "harmonic sequence is bounded in [0, 1]",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(is_bounded(harmonic, 100, 0, 1))`,
			expected: "True\n",
		},
		{
			name: "identity sequence is not bounded in [0, 5]",
			code: `{{FUNC}}
identity = lambda n: n
print(is_bounded(identity, 100, 0, 5))`,
			expected: "False\n",
		},
	],
};
