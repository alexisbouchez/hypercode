import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
	id: "hello-world",
	title: "Hello, World!",
	chapterId: "basics",
	content: `## Your First Ruby Program

Ruby's built-in method for printing output is \`puts\`:

\`\`\`ruby
puts "Hello, World!"
\`\`\`

\`puts\` prints the value and adds a newline at the end. You can also use \`print\` (no newline) and \`p\` (prints the inspect representation):

\`\`\`ruby
puts "hello"    # hello
print "hi"      # hi (no newline)
p 42            # 42
p "text"        # "text" (with quotes)
\`\`\`

### Comments

Single-line comments start with \`#\`:

\`\`\`ruby
# This is a comment
puts "code"  # This is also a comment
\`\`\`

### Your Task

Print exactly \`Hello, World!\` using \`puts\`.`,

	starterCode: `# Print "Hello, World!"
`,

	solution: `puts "Hello, World!"
`,

	tests: [
		{
			name: "prints Hello, World!",
			expected: "Hello, World!\n",
		},
	],
};
