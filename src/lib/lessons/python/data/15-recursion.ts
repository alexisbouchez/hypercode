import type { Lesson } from "../../types";

export const recursion: Lesson = {
	id: "recursion",
	title: "Recursion",
	chapterId: "advanced",
	content: `## Recursion

A recursive function calls itself. Every recursive function needs:
1. A **base case** — the simplest input that doesn't recurse
2. A **recursive case** — reduce the problem and recurse

### Classic Example: Factorial

\`\`\`python
def factorial(n):
    if n == 0:       # base case
        return 1
    return n * factorial(n - 1)  # recursive case

factorial(5)  # 120
\`\`\`

### Tree Traversal

Recursion shines on recursive data structures like trees and nested lists:

\`\`\`python
def depth(tree):
    if not isinstance(tree, list):
        return 0
    return 1 + max(depth(child) for child in tree)
\`\`\`

### Memoization

Avoid recomputing by caching results:

\`\`\`python
from functools import lru_cache

@lru_cache(maxsize=None)
def fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)
\`\`\`

### Mutual Recursion

Two functions calling each other:

\`\`\`python
def is_even(n):
    if n == 0: return True
    return is_odd(n - 1)

def is_odd(n):
    if n == 0: return False
    return is_even(n - 1)
\`\`\`

### Your Task

Implement \`flatten(nested)\` that recursively flattens an arbitrarily-nested list of integers into a single flat list.

\`\`\`python
flatten([1, [2, [3, 4], 5], 6])  # [1, 2, 3, 4, 5, 6]
flatten([[1, 2], [3, [4, [5]]]])  # [1, 2, 3, 4, 5]
\`\`\``,

	starterCode: `def flatten(nested):
    result = []
    for item in nested:
        if isinstance(item, list):
            # Recurse: extend result with flattened item
            pass
        else:
            result.append(item)
    return result

print(flatten([1, [2, [3, 4], 5], 6]))
print(flatten([[1, 2], [3, [4, [5]]]]))
print(flatten([1, 2, 3]))
`,

	solution: `def flatten(nested):
    result = []
    for item in nested:
        if isinstance(item, list):
            result.extend(flatten(item))
        else:
            result.append(item)
    return result

print(flatten([1, [2, [3, 4], 5], 6]))
print(flatten([[1, 2], [3, [4, [5]]]]))
print(flatten([1, 2, 3]))
`,

	tests: [
		{
			name: "flatten one level",
			code: `{{FUNC}}
print(flatten([1, 2, 3]))`,
			expected: "[1, 2, 3]\n",
		},
		{
			name: "flatten two levels",
			code: `{{FUNC}}
print(flatten([1, [2, 3], 4]))`,
			expected: "[1, 2, 3, 4]\n",
		},
		{
			name: "flatten deeply nested",
			code: `{{FUNC}}
print(flatten([1, [2, [3, 4], 5], 6]))`,
			expected: "[1, 2, 3, 4, 5, 6]\n",
		},
		{
			name: "flatten empty list",
			code: `{{FUNC}}
print(flatten([]))`,
			expected: "[]\n",
		},
	],
};
