import type { Lesson } from "../../types";

export const blocks: Lesson = {
	id: "blocks",
	title: "Blocks",
	chapterId: "methods_oop",
	content: `## Blocks in Ruby

A **block** is a chunk of code you pass to a method, written between \`{ }\` or \`do...end\`:

\`\`\`ruby
[1, 2, 3].each { |n| puts n }

[1, 2, 3].each do |n|
  puts n
end
\`\`\`

Both forms are equivalent. Convention: use \`{ }\` for one-liners, \`do...end\` for multi-line.

### yield

Inside a method, use \`yield\` to call the block that was passed:

\`\`\`ruby
def run_twice
  yield
  yield
end

run_twice { puts "hello" }
# hello
# hello
\`\`\`

### block_given?

Check if a block was provided before yielding:

\`\`\`ruby
def maybe_run
  if block_given?
    yield
  else
    puts "no block"
  end
end
\`\`\`

### Passing Values to the Block

\`yield\` can pass arguments to the block:

\`\`\`ruby
def repeat(n)
  n.times { |i| yield i }
end

repeat(3) { |i| puts "step #{i}" }
# step 0
# step 1
# step 2
\`\`\`

### Your Task

Write a method \`apply_twice\` that calls the block twice using \`yield\`.

Then call it with a block that \`puts "Hello!"\`.`,

	starterCode: `def apply_twice
  # yield twice
end

apply_twice { puts "Hello!" }
`,

	solution: `def apply_twice
  yield
  yield
end

apply_twice { puts "Hello!" }
`,

	tests: [
		{
			name: "Hello! twice",
			expected: "Hello!\nHello!\n",
		},
	],
};
