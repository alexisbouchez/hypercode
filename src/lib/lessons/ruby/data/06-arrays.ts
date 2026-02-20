import type { Lesson } from "../../types";

export const arrays: Lesson = {
	id: "arrays",
	title: "Arrays",
	chapterId: "collections",
	content: `## Arrays in Ruby

An array is an ordered list of values, created with square brackets:

\`\`\`ruby
fruits = ["apple", "banana", "cherry"]
numbers = [1, 2, 3, 4, 5]
mixed = [1, "hello", true, nil]
\`\`\`

### Accessing Elements

Arrays are zero-indexed. Negative indices count from the end:

\`\`\`ruby
fruits = ["apple", "banana", "cherry"]
puts fruits[0]    # apple
puts fruits[-1]   # cherry (last element)
puts fruits.first # apple
puts fruits.last  # cherry
\`\`\`

### Common Methods

\`\`\`ruby
numbers = [1, 2, 3]
puts numbers.length   # 3
numbers.push(4)       # add to end: [1, 2, 3, 4]
numbers.pop           # remove from end: [1, 2, 3]
puts numbers.join(", ")  # "1, 2, 3"
puts numbers.reverse.inspect  # [3, 2, 1]
puts numbers.sort.inspect     # [1, 2, 3]
puts numbers.include?(2)      # true
\`\`\`

### Your Task

Starting with \`numbers = [1, 2, 3, 4, 5]\`:
1. Print \`numbers[0]\`
2. Print \`numbers.length\`
3. Push \`6\` to the array
4. Print \`numbers.last\`
5. Print \`numbers.join(", ")\``,

	starterCode: `numbers = [1, 2, 3, 4, 5]
# 1. Print numbers[0]
# 2. Print numbers.length
# 3. Push 6
# 4. Print numbers.last
# 5. Print numbers.join(", ")
`,

	solution: `numbers = [1, 2, 3, 4, 5]
puts numbers[0]
puts numbers.length
numbers.push(6)
puts numbers.last
puts numbers.join(", ")
`,

	tests: [
		{
			name: "1, 5, 6, 1 2 3 4 5 6",
			expected: "1\n5\n6\n1, 2, 3, 4, 5, 6\n",
		},
	],
};
