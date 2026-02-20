import type { Lesson } from "../../types";

export const loops: Lesson = {
	id: "loops",
	title: "Loops",
	chapterId: "control_flow",
	content: `## Loops in Ruby

### while

Runs as long as a condition is true:

\`\`\`ruby
i = 1
while i <= 5
  puts i
  i += 1
end
\`\`\`

### until

The opposite of \`while\` — runs until the condition becomes true:

\`\`\`ruby
i = 1
until i > 5
  puts i
  i += 1
end
\`\`\`

### loop with break

\`loop\` runs forever; use \`break\` to exit:

\`\`\`ruby
i = 0
loop do
  i += 1
  break if i >= 5
end
puts i  # 5
\`\`\`

### times

The \`times\` method on integers is the most Ruby-idiomatic way to repeat something N times:

\`\`\`ruby
3.times { puts "hello" }
\`\`\`

### Your Task

Implement \`factorial(n)\` using a \`while\` loop.
The factorial of n (written n!) is the product of all integers from 1 to n.
- \`factorial(0)\` = 1
- \`factorial(5)\` = 120

Print \`factorial(5)\` and \`factorial(10)\`.`,

	starterCode: `def factorial(n)
  # Use a while loop
end

puts factorial(5)
puts factorial(10)
`,

	solution: `def factorial(n)
  result = 1
  i = 1
  while i <= n
    result *= i
    i += 1
  end
  result
end

puts factorial(5)
puts factorial(10)
`,

	tests: [
		{
			name: "factorial(5)=120, factorial(10)=3628800",
			expected: "120\n3628800\n",
		},
		{
			name: "factorial(0) = 1",
			code: `{{FUNC}}\nputs factorial(0)`,
			expected: "1\n",
		},
		{
			name: "factorial(1) = 1",
			code: `{{FUNC}}\nputs factorial(1)`,
			expected: "1\n",
		},
	],
};
