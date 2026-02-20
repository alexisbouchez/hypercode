import type { Lesson } from "../../types";

export const numbers: Lesson = {
	id: "numbers",
	title: "Numbers",
	chapterId: "numbers",
	content: `## Numbers in Ruby

Ruby has two main numeric types: **Integer** and **Float**.

### Integer Arithmetic

\`\`\`ruby
puts 2 + 3    # 5
puts 10 - 4   # 6
puts 3 * 7    # 21
puts 2 ** 8   # 256  (exponentiation)
puts 17 / 5   # 3    (integer division — truncates)
puts 17 % 5   # 2    (remainder / modulo)
\`\`\`

Integer division **truncates** toward zero — \`17 / 5\` is \`3\`, not \`3.4\`.

### Float Arithmetic

Use a float operand to get a float result:

\`\`\`ruby
puts 10.0 / 4   # 2.5
puts 1.5 + 2.5  # 4.0
\`\`\`

### Useful Methods

\`\`\`ruby
puts -7.abs      # 7   (absolute value — careful: -7.abs is -(7.abs) = -7!)
puts (-7).abs    # 7   (correct)
puts 3.14.round  # 3
puts 3.14.ceil   # 4
puts 3.14.floor  # 3
\`\`\`

### Your Task

Print the results of:
1. \`2 ** 10\`
2. \`10 / 3\`
3. \`10.0 / 4\`
4. \`17 % 5\``,

	starterCode: `# Print: 2**10, 10/3, 10.0/4, 17%5
`,

	solution: `puts 2 ** 10
puts 10 / 3
puts 10.0 / 4
puts 17 % 5
`,

	tests: [
		{
			name: "1024, 3, 2.5, 2",
			expected: "1024\n3\n2.5\n2\n",
		},
	],
};
