import type { Lesson } from "../../types";

export const methods: Lesson = {
	id: "methods",
	title: "Methods",
	chapterId: "methods_oop",
	content: `## Methods in Ruby

Define a method with \`def\` and close it with \`end\`:

\`\`\`ruby
def greet(name)
  puts "Hello, #{name}!"
end

greet("Alice")  # Hello, Alice!
\`\`\`

### Return Values

The last expression in a method is **automatically returned** — no \`return\` keyword needed:

\`\`\`ruby
def square(n)
  n * n
end

puts square(5)  # 25
\`\`\`

You can also use \`return\` explicitly to exit early.

### Default Parameters

Parameters can have default values:

\`\`\`ruby
def greet(name, greeting = "Hello")
  "#{greeting}, #{name}!"
end

puts greet("Alice")         # Hello, Alice!
puts greet("Bob", "Hi")    # Hi, Bob!
\`\`\`

### Your Task

Write a method \`full_name(first, last, separator = " ")\` that returns the full name with the separator between first and last.

Print \`full_name("John", "Doe")\` and \`full_name("John", "Doe", "_")\`.`,

	starterCode: `def full_name(first, last, separator = " ")
  # Return "first separator last"
end

puts full_name("John", "Doe")
puts full_name("John", "Doe", "_")
`,

	solution: `def full_name(first, last, separator = " ")
  "#{first}#{separator}#{last}"
end

puts full_name("John", "Doe")
puts full_name("John", "Doe", "_")
`,

	tests: [
		{
			name: "John Doe, John_Doe",
			expected: "John Doe\nJohn_Doe\n",
		},
		{
			name: 'full_name("Alice", "Smith") = "Alice Smith"',
			code: `{{FUNC}}\nputs full_name("Alice", "Smith")`,
			expected: "Alice Smith\n",
		},
		{
			name: 'full_name("a", "b", "-") = "a-b"',
			code: `{{FUNC}}\nputs full_name("a", "b", "-")`,
			expected: "a-b\n",
		},
	],
};
