import type { Lesson } from "../../types";

export const variables: Lesson = {
	id: "variables",
	title: "Variables",
	chapterId: "basics",
	content: `## Variables in Ruby

Ruby variables are assigned with \`=\`. You don't declare types — Ruby figures them out:

\`\`\`ruby
name = "Alice"    # String
age = 30          # Integer
height = 5.8      # Float
active = true     # Boolean
\`\`\`

Ruby has four main scalar types:
- **String** — text in double or single quotes: \`"hello"\` or \`'hello'\`
- **Integer** — whole numbers: \`42\`, \`-7\`, \`1_000_000\`
- **Float** — decimal numbers: \`3.14\`, \`-0.5\`
- **Boolean** — \`true\` or \`false\`

There is also \`nil\`, Ruby's "nothing" value.

\`puts\` converts any value to a string before printing:

\`\`\`ruby
puts 42      # "42"
puts true    # "true"
puts nil     # (empty line)
\`\`\`

### Your Task

Create four variables:
- \`name\` = \`"Alice"\`
- \`age\` = \`30\`
- \`height\` = \`5.8\`
- \`active\` = \`true\`

Print each on its own line.`,

	starterCode: `name = ""
age = 0
height = 0.0
active = false
# Print each variable on its own line
`,

	solution: `name = "Alice"
age = 30
height = 5.8
active = true
puts name
puts age
puts height
puts active
`,

	tests: [
		{
			name: "prints all four variables",
			expected: "Alice\n30\n5.8\ntrue\n",
		},
	],
};
