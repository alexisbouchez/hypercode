import type { Lesson } from "../../types";

export const ranges: Lesson = {
	id: "ranges",
	title: "Ranges",
	chapterId: "collections",
	content: `## Ranges in Ruby

A range represents a sequence of values between two endpoints:

\`\`\`ruby
(1..5)    # inclusive: 1, 2, 3, 4, 5
(1...5)   # exclusive: 1, 2, 3, 4  (excludes 5)
('a'..'e')  # character range: a, b, c, d, e
\`\`\`

Two dots \`...\` means "up to but not including" the end.

### Common Operations

\`\`\`ruby
r = (1..5)
puts r.include?(3)       # true
puts r.to_a.inspect      # [1, 2, 3, 4, 5]
puts r.min               # 1
puts r.max               # 5
puts r.sum               # 15
puts r.count             # 5
\`\`\`

### Iterating

Ranges work naturally with \`each\`:

\`\`\`ruby
(1..3).each { |n| puts n }
# 1
# 2
# 3
\`\`\`

### Your Task

Print four values:
1. Length of \`(1..5)\` as an array
2. Length of \`(1...5)\` as an array
3. Whether \`7\` is in \`(1..10)\`
4. Sum of \`(1..100)\``,

	starterCode: `# Print: length of 1..5, length of 1...5, 7 in 1..10, sum of 1..100
`,

	solution: `puts (1..5).to_a.length
puts (1...5).to_a.length
puts (1..10).include?(7)
puts (1..100).sum
`,

	tests: [
		{
			name: "5, 4, true, 5050",
			expected: "5\n4\ntrue\n5050\n",
		},
	],
};
