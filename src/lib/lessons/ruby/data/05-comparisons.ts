import type { Lesson } from "../../types";

export const comparisons: Lesson = {
	id: "comparisons",
	title: "Comparisons",
	chapterId: "numbers",
	content: `## Comparisons and Booleans

Comparison operators return \`true\` or \`false\`:

\`\`\`ruby
puts 5 > 3    # true
puts 5 < 3    # false
puts 5 >= 5   # true
puts 5 <= 4   # false
puts 5 == 5   # true  (equality)
puts 5 != 4   # true  (not equal)
\`\`\`

### Logical Operators

\`\`\`ruby
puts true && false   # false  (AND)
puts true || false   # true   (OR)
puts !true           # false  (NOT)
\`\`\`

Ruby also has the word forms \`and\`, \`or\`, \`not\` — but prefer \`&&\`, \`||\`, \`!\` in expressions.

### Truthiness

In Ruby, **only \`false\` and \`nil\` are falsy**. Everything else — including \`0\` and \`""\` — is truthy:

\`\`\`ruby
puts !!0    # true  (0 is truthy in Ruby!)
puts !!nil  # false
\`\`\`

### Your Task

Print the result of each expression:
1. \`5 > 3\`
2. \`5 == 5\`
3. \`5 != 4\`
4. \`3 <= 3\`
5. \`true && false\`
6. \`true || false\`
7. \`!true\``,

	starterCode: `# Print results of seven comparison/logical expressions
`,

	solution: `puts 5 > 3
puts 5 == 5
puts 5 != 4
puts 3 <= 3
puts true && false
puts true || false
puts !true
`,

	tests: [
		{
			name: "true, true, true, true, false, true, false",
			expected: "true\ntrue\ntrue\ntrue\nfalse\ntrue\nfalse\n",
		},
	],
};
