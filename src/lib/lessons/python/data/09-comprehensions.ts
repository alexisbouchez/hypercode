import type { Lesson } from "../../types";

export const comprehensions: Lesson = {
	id: "comprehensions",
	title: "Comprehensions",
	chapterId: "control-and-iteration",
	content: `## Comprehensions

Comprehensions are a concise, Pythonic way to build collections from iterables.

### List Comprehension

\`\`\`python
squares = [x**2 for x in range(10)]
# [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]

evens = [x for x in range(20) if x % 2 == 0]
# [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]
\`\`\`

### Dict Comprehension

\`\`\`python
words = ["hello", "world", "python"]
lengths = {w: len(w) for w in words}
# {'hello': 5, 'world': 5, 'python': 6}
\`\`\`

### Set Comprehension

\`\`\`python
unique_lens = {len(w) for w in words}
# {5, 6}
\`\`\`

### Nested Comprehension

\`\`\`python
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flat = [x for row in matrix for x in row]
# [1, 2, 3, 4, 5, 6, 7, 8, 9]
\`\`\`

### When to Use

Comprehensions shine for simple transformations and filters. For complex logic, a regular \`for\` loop is more readable.

### Your Task

Implement \`primes_up_to(n)\` that returns a sorted list of all prime numbers from 2 to \`n\` (inclusive), using a list comprehension with a helper or direct logic.`,

	starterCode: `def primes_up_to(n):
    # Return a list of all primes from 2 to n inclusive
    # Hint: a number p is prime if no integer from 2 to p-1 divides it
    pass

print(primes_up_to(10))
print(primes_up_to(20))
print(len(primes_up_to(100)))
`,

	solution: `def primes_up_to(n):
    return [p for p in range(2, n + 1)
            if all(p % d != 0 for d in range(2, p))]

print(primes_up_to(10))
print(primes_up_to(20))
print(len(primes_up_to(100)))
`,

	tests: [
		{
			name: "primes_up_to(10)",
			code: `{{FUNC}}
print(primes_up_to(10))`,
			expected: "[2, 3, 5, 7]\n",
		},
		{
			name: "primes_up_to(2)",
			code: `{{FUNC}}
print(primes_up_to(2))`,
			expected: "[2]\n",
		},
		{
			name: "primes_up_to(1) is empty",
			code: `{{FUNC}}
print(primes_up_to(1))`,
			expected: "[]\n",
		},
		{
			name: "25 primes up to 100",
			code: `{{FUNC}}
print(len(primes_up_to(100)))`,
			expected: "25\n",
		},
	],
};
