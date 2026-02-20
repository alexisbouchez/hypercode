import type { Lesson } from "../../types";

export const standardLibrary: Lesson = {
	id: "standard-library",
	title: "Standard Library",
	chapterId: "advanced",
	content: `## Standard Library

Python's standard library is vast. A few essential modules:

### collections

\`Counter\` counts hashable objects:

\`\`\`python
from collections import Counter

words = ["apple", "banana", "apple", "cherry", "banana", "apple"]
c = Counter(words)
c["apple"]           # 3
c.most_common(2)     # [('apple', 3), ('banana', 2)]
\`\`\`

\`defaultdict\` returns a default value for missing keys:

\`\`\`python
from collections import defaultdict

groups = defaultdict(list)
groups["a"].append(1)   # no KeyError
\`\`\`

### itertools

\`\`\`python
from itertools import chain, groupby, product, combinations

list(chain([1, 2], [3, 4]))       # [1, 2, 3, 4]
list(combinations("ABC", 2))      # [('A','B'), ('A','C'), ('B','C')]
list(product([0,1], repeat=2))    # [(0,0), (0,1), (1,0), (1,1)]
\`\`\`

### functools

\`\`\`python
from functools import reduce, lru_cache

reduce(lambda a, b: a + b, [1, 2, 3, 4])  # 10

@lru_cache(maxsize=None)
def fib(n):
    if n < 2: return n
    return fib(n-1) + fib(n-2)
\`\`\`

### Your Task

Implement \`most_common_words(text, n)\` that:
- Splits \`text\` into words (by spaces), lowercases all words
- Returns a list of the \`n\` most common words as \`(word, count)\` tuples, sorted by frequency (most frequent first)

Use \`collections.Counter\`.`,

	starterCode: `from collections import Counter

def most_common_words(text, n):
    # Split, lowercase, count, return top n
    pass

result = most_common_words("the cat sat on the mat the cat", 2)
for word, count in result:
    print(f"{word}: {count}")
`,

	solution: `from collections import Counter

def most_common_words(text, n):
    words = text.lower().split()
    counter = Counter(words)
    return counter.most_common(n)

result = most_common_words("the cat sat on the mat the cat", 2)
for word, count in result:
    print(f"{word}: {count}")
`,

	tests: [
		{
			name: "top 1 word is 'the'",
			code: `{{FUNC}}
result = most_common_words("the cat sat on the mat the cat", 1)
print(result[0][0])
print(result[0][1])`,
			expected: "the\n3\n",
		},
		{
			name: "top 2 words returned",
			code: `{{FUNC}}
result = most_common_words("the cat sat on the mat the cat", 2)
print(len(result))`,
			expected: "2\n",
		},
		{
			name: "case insensitive",
			code: `{{FUNC}}
result = most_common_words("Hello hello HELLO world", 1)
print(result[0][0])
print(result[0][1])`,
			expected: "hello\n3\n",
		},
		{
			name: "single word text",
			code: `{{FUNC}}
result = most_common_words("python", 1)
print(result[0][0])`,
			expected: "python\n",
		},
	],
};
