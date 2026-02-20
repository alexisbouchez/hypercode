import type { Lesson } from "../../types";

export const iterators: Lesson = {
	id: "iterators",
	title: "Iterators",
	chapterId: "control_flow",
	content: `## Iterators in Ruby

Ruby's arrays come with powerful iterator methods that take a **block** — code between \`{ }\` or \`do...end\`:

### each

Runs a block for every element:

\`\`\`ruby
[1, 2, 3].each { |n| puts n * 2 }
# 2
# 4
# 6
\`\`\`

### map

Transforms each element, returns a new array:

\`\`\`ruby
squares = [1, 2, 3, 4].map { |n| n ** 2 }
puts squares.inspect  # [1, 4, 9, 16]
\`\`\`

### select / reject

Filter elements based on a condition:

\`\`\`ruby
evens = (1..10).select { |n| n.even? }
odds = (1..10).reject { |n| n.even? }
puts evens.inspect  # [2, 4, 6, 8, 10]
\`\`\`

### reduce

Accumulate a value across all elements:

\`\`\`ruby
total = [1, 2, 3, 4, 5].reduce(0) { |sum, n| sum + n }
puts total  # 15
\`\`\`

### Your Task

Given \`numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\`:
1. Use \`map\` to double each number; print joined with \`", "\`
2. Use \`select\` to get even numbers; print joined with \`", "\`
3. Use \`reduce\` to sum all numbers; print the total`,

	starterCode: `numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# 1. doubles = map { |n| n * 2 }, print joined
# 2. evens = select { |n| n.even? }, print joined
# 3. total = reduce(0) { |sum, n| sum + n }, print
`,

	solution: `numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
doubles = numbers.map { |n| n * 2 }
puts doubles.join(", ")
evens = numbers.select { |n| n.even? }
puts evens.join(", ")
total = numbers.reduce(0) { |sum, n| sum + n }
puts total
`,

	tests: [
		{
			name: "doubles, evens, sum",
			expected: "2, 4, 6, 8, 10, 12, 14, 16, 18, 20\n2, 4, 6, 8, 10\n55\n",
		},
	],
};
