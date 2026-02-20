import type { Lesson } from "../../types";

export const helloPython: Lesson = {
	id: "hello-python",
	title: "Hello Python",
	chapterId: "foundations",
	content: `## Hello Python

Python is a high-level, dynamically typed language famous for its readable syntax. There are no semicolons, no braces — indentation defines structure.

### print()

\`\`\`python
print("Hello, world!")
print(42)
print(3.14)
print(True)
\`\`\`

### Variables

Python infers types automatically. No declarations needed.

\`\`\`python
name = "Alice"
age = 30
height = 1.75
active = True
\`\`\`

### f-strings

The cleanest way to embed variables in strings (Python 3.6+):

\`\`\`python
name = "Alice"
age = 30
print(f"My name is {name} and I am {age} years old.")
\`\`\`

### Multiple Values

\`print()\` accepts multiple arguments separated by commas, joined with spaces by default:

\`\`\`python
print("Score:", 42, "/ 100")
# Score: 42 / 100
\`\`\`

### Your Task

Implement \`greet(name)\` that prints \`Hello, {name}!\` on one line.`,

	starterCode: `def greet(name):
    # Print "Hello, {name}!" using an f-string
    pass

greet("Alice")
greet("World")
`,

	solution: `def greet(name):
    print(f"Hello, {name}!")

greet("Alice")
greet("World")
`,

	tests: [
		{
			name: "greet prints Hello, Alice!",
			code: `{{FUNC}}
greet("Alice")`,
			expected: "Hello, Alice!\n",
		},
		{
			name: "greet prints Hello, World!",
			code: `{{FUNC}}
greet("World")`,
			expected: "Hello, World!\n",
		},
		{
			name: "greet with single-word name",
			code: `{{FUNC}}
greet("Bob")`,
			expected: "Hello, Bob!\n",
		},
		{
			name: "greet with multi-word name",
			code: `{{FUNC}}
greet("John Doe")`,
			expected: "Hello, John Doe!\n",
		},
	],
};
