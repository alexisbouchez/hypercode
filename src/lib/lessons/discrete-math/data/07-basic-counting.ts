import type { Lesson } from "../../types";

export const basicCounting: Lesson = {
	id: "basic-counting",
	title: "Basic Counting",
	chapterId: "counting-combinatorics",
	content: `## The Art of Counting

Combinatorics answers the question: **how many ways can something happen?**

### Fundamental Rules

**Product rule**: If task A can be done in $m$ ways and task B in $n$ ways independently, together there are $m \\cdot n$ ways.

**Sum rule**: If task A can be done in $m$ ways OR task B in $n$ ways (mutually exclusive), there are $m + n$ ways total.

### Permutations and Combinations

**Permutation** $P(n, r)$ — ordered arrangements of $r$ items from $n$:
$$P(n, r) = \\frac{n!}{(n-r)!}$$

**Combination** $C(n, r) = \\binom{n}{r}$ — unordered selections of $r$ items from $n$:
$$\\binom{n}{r} = \\frac{n!}{r!(n-r)!}$$

**Arrangements with repetition** — $r$ choices from $n$ options with replacement: $n^r$

\`\`\`python
import math

def permutations(n, r):
    return math.factorial(n) // math.factorial(n - r)

def combinations(n, r):
    return math.factorial(n) // (math.factorial(r) * math.factorial(n - r))

def arrangements_with_repetition(n, r):
    return n ** r

print(permutations(5, 3))   # 60  (5×4×3)
print(combinations(10, 3))  # 120
\`\`\`

### Your Task

Implement \`permutations(n, r)\`, \`combinations(n, r)\`, and \`arrangements_with_repetition(n, r)\`.`,

	starterCode: `import math

def permutations(n, r):
    # n! / (n-r)!
    pass

def combinations(n, r):
    # n! / (r! * (n-r)!)
    pass

def arrangements_with_repetition(n, r):
    # n^r
    pass

print(permutations(5, 3))   # 60
print(combinations(10, 3))  # 120
`,

	solution: `import math

def permutations(n, r):
    return math.factorial(n) // math.factorial(n - r)

def combinations(n, r):
    return math.factorial(n) // (math.factorial(r) * math.factorial(n - r))

def arrangements_with_repetition(n, r):
    return n ** r

print(permutations(5, 3))
print(combinations(10, 3))
`,

	tests: [
		{
			name: "P(5,3) = 60",
			code: `{{FUNC}}
print(permutations(5, 3))`,
			expected: "60\n",
		},
		{
			name: "C(10,3) = 120",
			code: `{{FUNC}}
print(combinations(10, 3))`,
			expected: "120\n",
		},
		{
			name: "2^8 binary strings = 256",
			code: `{{FUNC}}
print(arrangements_with_repetition(2, 8))`,
			expected: "256\n",
		},
		{
			name: "C(52,5) poker hands = 2598960",
			code: `{{FUNC}}
print(combinations(52, 5))`,
			expected: "2598960\n",
		},
	],
};
