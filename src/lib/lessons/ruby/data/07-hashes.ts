import type { Lesson } from "../../types";

export const hashes: Lesson = {
	id: "hashes",
	title: "Hashes",
	chapterId: "collections",
	content: `## Hashes in Ruby

A hash is a collection of key-value pairs, similar to a dictionary. Keys are most often **symbols** (prefixed with \`:\`):

\`\`\`ruby
person = { name: "Alice", age: 30, city: "Paris" }
puts person[:name]   # Alice
puts person[:age]    # 30
\`\`\`

You can also use strings as keys, but symbols are more common:

\`\`\`ruby
config = { "host" => "localhost", "port" => 5432 }
puts config["host"]  # localhost
\`\`\`

### Adding and Updating

\`\`\`ruby
person = { name: "Alice", age: 30 }
person[:city] = "Paris"     # add new key
person[:age] = 31           # update existing key
\`\`\`

### Common Methods

\`\`\`ruby
h = { a: 1, b: 2, c: 3 }
puts h.keys.inspect    # [:a, :b, :c]
puts h.values.inspect  # [1, 2, 3]
puts h.length          # 3
puts h.key?(:a)        # true
\`\`\`

### Your Task

Starting with \`person = { name: "Alice", age: 30, city: "Paris" }\`:
1. Print \`person[:name]\`
2. Print \`person[:age]\`
3. Add key \`country\` with value \`"France"\`
4. Print \`person.keys.length\`
5. Print \`person[:country]\``,

	starterCode: `person = { name: "Alice", age: 30, city: "Paris" }
# 1. Print person[:name]
# 2. Print person[:age]
# 3. Add :country => "France"
# 4. Print person.keys.length
# 5. Print person[:country]
`,

	solution: `person = { name: "Alice", age: 30, city: "Paris" }
puts person[:name]
puts person[:age]
person[:country] = "France"
puts person.keys.length
puts person[:country]
`,

	tests: [
		{
			name: "Alice, 30, 4, France",
			expected: "Alice\n30\n4\nFrance\n",
		},
	],
};
