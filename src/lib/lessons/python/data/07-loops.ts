import type { Lesson } from "../../types";

export const loops: Lesson = {
	id: "loops",
	title: "Loops",
	chapterId: "control-and-iteration",
	content: `## Loops

Python has \`for\` and \`while\` loops. \`for\` iterates over any iterable.

### for Loop

\`\`\`python
for i in range(5):         # 0, 1, 2, 3, 4
    print(i)

for item in ["a", "b"]:   # iterate a list
    print(item)

for i, v in enumerate(["a", "b"]):  # index + value
    print(i, v)
\`\`\`

### range()

\`\`\`python
range(5)        # 0, 1, 2, 3, 4
range(1, 6)     # 1, 2, 3, 4, 5
range(0, 10, 2) # 0, 2, 4, 6, 8
range(5, 0, -1) # 5, 4, 3, 2, 1
\`\`\`

### while Loop

\`\`\`python
n = 10
while n > 0:
    print(n)
    n //= 2
\`\`\`

### break and continue

\`\`\`python
for i in range(10):
    if i == 5:
        break      # exit loop
    if i % 2 == 0:
        continue   # skip to next iteration
    print(i)
\`\`\`

### Your Task

Implement \`collatz(n)\` that:
- Returns the length of the Collatz sequence starting from \`n\`
- The sequence: if \`n\` is even, next = \`n // 2\`; if odd, next = \`3 * n + 1\`; stop when \`n == 1\`
- Count \`n\` itself as the first step

Example: \`collatz(6)\` → 6 → 3 → 10 → 5 → 16 → 8 → 4 → 2 → 1 → length **9**`,

	starterCode: `def collatz(n):
    # Count steps in Collatz sequence from n to 1 (inclusive)
    length = 1
    while n != 1:
        # Apply collatz rule
        pass
    return length

print(collatz(1))
print(collatz(6))
print(collatz(27))
`,

	solution: `def collatz(n):
    length = 1
    while n != 1:
        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1
        length += 1
    return length

print(collatz(1))
print(collatz(6))
print(collatz(27))
`,

	tests: [
		{
			name: "collatz(1) = 1",
			code: `{{FUNC}}
print(collatz(1))`,
			expected: "1\n",
		},
		{
			name: "collatz(6) = 9",
			code: `{{FUNC}}
print(collatz(6))`,
			expected: "9\n",
		},
		{
			name: "collatz(4) = 3",
			code: `{{FUNC}}
print(collatz(4))`,
			expected: "3\n",
		},
		{
			name: "collatz(27) = 112",
			code: `{{FUNC}}
print(collatz(27))`,
			expected: "112\n",
		},
	],
};
