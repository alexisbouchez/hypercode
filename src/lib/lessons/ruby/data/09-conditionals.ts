import type { Lesson } from "../../types";

export const conditionals: Lesson = {
	id: "conditionals",
	title: "Conditionals",
	chapterId: "control_flow",
	content: `## Conditionals in Ruby

Ruby's \`if\`/\`elsif\`/\`else\` works like most languages:

\`\`\`ruby
score = 85

if score >= 90
  puts "A"
elsif score >= 80
  puts "B"
elsif score >= 70
  puts "C"
else
  puts "F"
end
\`\`\`

### unless

\`unless\` is the opposite of \`if\` — it runs when the condition is **false**:

\`\`\`ruby
unless score < 60
  puts "Passing"
end
\`\`\`

### Inline (Modifier) Form

You can put \`if\` or \`unless\` at the end of a statement:

\`\`\`ruby
puts "Positive" if x > 0
puts "Not zero" unless x == 0
\`\`\`

### Ternary Operator

\`\`\`ruby
status = score >= 60 ? "pass" : "fail"
\`\`\`

### Your Task

Write a method \`grade(score)\` that returns:
- \`"A"\` for scores ≥ 90
- \`"B"\` for scores ≥ 80
- \`"C"\` for scores ≥ 70
- \`"F"\` otherwise

Then print \`grade(95)\`, \`grade(82)\`, \`grade(71)\`, \`grade(60)\`.`,

	starterCode: `def grade(score)
  # Return "A", "B", "C", or "F"
end

puts grade(95)
puts grade(82)
puts grade(71)
puts grade(60)
`,

	solution: `def grade(score)
  if score >= 90
    "A"
  elsif score >= 80
    "B"
  elsif score >= 70
    "C"
  else
    "F"
  end
end

puts grade(95)
puts grade(82)
puts grade(71)
puts grade(60)
`,

	tests: [
		{
			name: "A, B, C, F",
			expected: "A\nB\nC\nF\n",
		},
		{
			name: "grade(100) = A",
			code: `{{FUNC}}\nputs grade(100)`,
			expected: "A\n",
		},
		{
			name: "grade(50) = F",
			code: `{{FUNC}}\nputs grade(50)`,
			expected: "F\n",
		},
	],
};
