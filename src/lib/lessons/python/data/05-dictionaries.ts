import type { Lesson } from "../../types";

export const dictionaries: Lesson = {
	id: "dictionaries",
	title: "Dictionaries",
	chapterId: "foundations",
	content: `## Dictionaries

A Python dictionary stores key-value pairs. Keys must be hashable (strings, numbers, tuples). Dictionaries preserve insertion order since Python 3.7.

### Creating and Accessing

\`\`\`python
person = {"name": "Alice", "age": 30}
person["name"]          # "Alice"
person.get("age")       # 30
person.get("email", "") # "" (default if missing)
\`\`\`

### Mutating

\`\`\`python
person["email"] = "alice@example.com"  # add/update
del person["age"]                       # delete key
\`\`\`

### Iterating

\`\`\`python
for key in person:              # iterate keys
for key, val in person.items(): # iterate pairs
for val in person.values():     # iterate values
\`\`\`

### Useful Methods

\`\`\`python
person.keys()    # dict_keys(['name', 'email'])
person.values()  # dict_values(['Alice', 'alice@...'])
person.items()   # dict_items([('name', 'Alice'), ...])
"name" in person # True
\`\`\`

### setdefault and defaultdict

\`\`\`python
d = {}
d.setdefault("count", 0)  # set only if missing
d["count"] += 1
\`\`\`

### Your Task

Implement \`word_count(text)\` that returns a dictionary mapping each word to how many times it appears in \`text\`. Words are separated by spaces; treat them case-insensitively (lowercase all words).`,

	starterCode: `def word_count(text):
    counts = {}
    # Split text into words, lowercase each, count occurrences
    return counts

counts = word_count("the cat sat on the mat the cat")
print(counts["the"])
print(counts["cat"])
print(counts["sat"])
`,

	solution: `def word_count(text):
    counts = {}
    for word in text.lower().split():
        counts[word] = counts.get(word, 0) + 1
    return counts

counts = word_count("the cat sat on the mat the cat")
print(counts["the"])
print(counts["cat"])
print(counts["sat"])
`,

	tests: [
		{
			name: "'the' appears 3 times",
			code: `{{FUNC}}
c = word_count("the cat sat on the mat the cat")
print(c["the"])`,
			expected: "3\n",
		},
		{
			name: "'cat' appears 2 times",
			code: `{{FUNC}}
c = word_count("the cat sat on the mat the cat")
print(c["cat"])`,
			expected: "2\n",
		},
		{
			name: "case-insensitive: 'Hello hello'",
			code: `{{FUNC}}
c = word_count("Hello hello HELLO")
print(c["hello"])`,
			expected: "3\n",
		},
		{
			name: "single word",
			code: `{{FUNC}}
c = word_count("python")
print(c["python"])`,
			expected: "1\n",
		},
	],
};
