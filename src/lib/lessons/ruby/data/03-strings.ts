import type { Lesson } from "../../types";

export const strings: Lesson = {
	id: "strings",
	title: "Strings",
	chapterId: "basics",
	content: `## Strings in Ruby

Ruby strings support **interpolation** — embed any expression inside \`#{}\`:

\`\`\`ruby
name = "World"
puts "Hello, #{name}!"    # Hello, World!
puts "2 + 2 = #{2 + 2}"  # 2 + 2 = 4
\`\`\`

Use single quotes for literal strings (no interpolation):

\`\`\`ruby
puts 'Hello, #{name}!'   # Hello, #{name}! (literal)
\`\`\`

### Common String Methods

\`\`\`ruby
s = "hello"
puts s.upcase      # HELLO
puts s.downcase    # hello
puts s.length      # 5
puts s.reverse     # olleh
puts s.capitalize  # Hello
puts s.include?("ell")  # true
\`\`\`

### Your Task

Given \`name = "World"\`, print five lines:
1. \`Hello, World!\` (using interpolation)
2. \`name\` in uppercase
3. \`name\` in lowercase
4. The length of \`name\`
5. \`name\` reversed`,

	starterCode: `name = "World"
# Print: "Hello, World!", upcase, downcase, length, reverse
`,

	solution: `name = "World"
puts "Hello, #{name}!"
puts name.upcase
puts name.downcase
puts name.length
puts name.reverse
`,

	tests: [
		{
			name: "interpolation, upcase, downcase, length, reverse",
			expected: "Hello, World!\nWORLD\nworld\n5\ndlroW\n",
		},
	],
};
