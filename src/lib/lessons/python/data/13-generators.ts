import type { Lesson } from "../../types";

export const generators: Lesson = {
	id: "generators",
	title: "Generators",
	chapterId: "advanced",
	content: `## Generators

A generator is a function that produces values lazily using \`yield\`. Unlike lists, generators don't store all values in memory at once — they compute each value on demand.

### yield

\`\`\`python
def count_up(start, end):
    n = start
    while n <= end:
        yield n     # produce a value, then pause
        n += 1

for x in count_up(1, 5):
    print(x)  # 1, 2, 3, 4, 5
\`\`\`

### Infinite Generators

Generators can be infinite — only compute as many values as you need:

\`\`\`python
def naturals():
    n = 0
    while True:
        yield n
        n += 1
\`\`\`

### next()

Pull values one at a time with \`next()\`:

\`\`\`python
gen = count_up(1, 3)
next(gen)  # 1
next(gen)  # 2
next(gen)  # 3
next(gen)  # raises StopIteration
\`\`\`

### Generator Expressions

Like list comprehensions, but lazy:

\`\`\`python
squares = (x**2 for x in range(1_000_000))  # no memory cost
next(squares)  # 0
\`\`\`

### itertools

\`itertools\` has powerful tools for working with iterables: \`islice\`, \`chain\`, \`cycle\`, \`accumulate\`, etc.

\`\`\`python
from itertools import islice
first_five = list(islice(naturals(), 5))  # [0, 1, 2, 3, 4]
\`\`\`

### Your Task

Implement \`fibonacci()\` — an infinite generator that yields Fibonacci numbers starting from 0, 1, 1, 2, 3, 5, 8, 13, ...`,

	starterCode: `def fibonacci():
    # Yield Fibonacci numbers infinitely: 0, 1, 1, 2, 3, 5, 8, ...
    a, b = 0, 1
    while True:
        yield a
        # Update a and b
        pass

from itertools import islice
print(list(islice(fibonacci(), 8)))
`,

	solution: `def fibonacci():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b

from itertools import islice
print(list(islice(fibonacci(), 8)))
`,

	tests: [
		{
			name: "first 8 fibonacci numbers",
			code: `{{FUNC}}
from itertools import islice
print(list(islice(fibonacci(), 8)))`,
			expected: "[0, 1, 1, 2, 3, 5, 8, 13]\n",
		},
		{
			name: "first fibonacci number is 0",
			code: `{{FUNC}}
gen = fibonacci()
print(next(gen))`,
			expected: "0\n",
		},
		{
			name: "second fibonacci number is 1",
			code: `{{FUNC}}
gen = fibonacci()
next(gen)
print(next(gen))`,
			expected: "1\n",
		},
		{
			name: "10th fibonacci number is 34",
			code: `{{FUNC}}
from itertools import islice
vals = list(islice(fibonacci(), 10))
print(vals[9])`,
			expected: "34\n",
		},
	],
};
