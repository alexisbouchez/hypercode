import type { Lesson } from "../../types";

export const controlFlow: Lesson = {
	id: "control-flow",
	title: "Control Flow",
	chapterId: "control-and-iteration",
	content: `## Control Flow

Python uses indentation to define blocks. There are no braces or \`begin/end\` keywords.

### if / elif / else

\`\`\`python
x = 42
if x > 100:
    print("big")
elif x > 10:
    print("medium")
else:
    print("small")
\`\`\`

### Comparison Operators

\`\`\`python
x == y   # equal
x != y   # not equal
x < y    # less than
x > y    # greater than
x <= y   # less than or equal
x >= y   # greater than or equal
\`\`\`

### Logical Operators

\`\`\`python
True and False  # False
True or False   # True
not True        # False
\`\`\`

### Ternary Expression

\`\`\`python
label = "even" if n % 2 == 0 else "odd"
\`\`\`

### match (Python 3.10+)

\`\`\`python
match command:
    case "quit":
        quit()
    case "help":
        show_help()
    case _:
        print("unknown command")
\`\`\`

### Your Task

Implement \`fizzbuzz(n)\` that returns:
- \`"FizzBuzz"\` if \`n\` is divisible by both 3 and 5
- \`"Fizz"\` if divisible by 3
- \`"Buzz"\` if divisible by 5
- The string representation of \`n\` otherwise`,

	starterCode: `def fizzbuzz(n):
    # Return "FizzBuzz", "Fizz", "Buzz", or str(n)
    pass

print(fizzbuzz(15))
print(fizzbuzz(9))
print(fizzbuzz(10))
print(fizzbuzz(7))
`,

	solution: `def fizzbuzz(n):
    if n % 15 == 0:
        return "FizzBuzz"
    elif n % 3 == 0:
        return "Fizz"
    elif n % 5 == 0:
        return "Buzz"
    else:
        return str(n)

print(fizzbuzz(15))
print(fizzbuzz(9))
print(fizzbuzz(10))
print(fizzbuzz(7))
`,

	tests: [
		{
			name: "15 → FizzBuzz",
			code: `{{FUNC}}
print(fizzbuzz(15))`,
			expected: "FizzBuzz\n",
		},
		{
			name: "9 → Fizz",
			code: `{{FUNC}}
print(fizzbuzz(9))`,
			expected: "Fizz\n",
		},
		{
			name: "10 → Buzz",
			code: `{{FUNC}}
print(fizzbuzz(10))`,
			expected: "Buzz\n",
		},
		{
			name: "7 → '7'",
			code: `{{FUNC}}
print(fizzbuzz(7))`,
			expected: "7\n",
		},
	],
};
