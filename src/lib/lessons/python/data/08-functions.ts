import type { Lesson } from "../../types";

export const functions: Lesson = {
	id: "functions",
	title: "Functions",
	chapterId: "control-and-iteration",
	content: `## Functions

Functions are first-class objects in Python — they can be passed as arguments, returned from other functions, and stored in variables.

### def and return

\`\`\`python
def add(a, b):
    return a + b

result = add(3, 4)  # 7
\`\`\`

### Default Arguments

\`\`\`python
def greet(name, greeting="Hello"):
    return f"{greeting}, {name}!"

greet("Alice")           # "Hello, Alice!"
greet("Bob", "Hi")       # "Hi, Bob!"
\`\`\`

### *args and **kwargs

\`\`\`python
def total(*args):        # collects positional args into tuple
    return sum(args)

def info(**kwargs):      # collects keyword args into dict
    for k, v in kwargs.items():
        print(f"{k}: {v}")

total(1, 2, 3)           # 6
info(name="Alice", age=30)
\`\`\`

### Functions as Arguments

\`\`\`python
def apply(f, x):
    return f(x)

apply(abs, -5)           # 5
apply(str.upper, "hi")   # "HI"
\`\`\`

### Lambda

\`\`\`python
double = lambda x: x * 2
double(4)  # 8
\`\`\`

### Your Task

Implement \`apply_twice(f, x)\` that applies function \`f\` to \`x\` twice: \`f(f(x))\`.`,

	starterCode: `def apply_twice(f, x):
    # Apply f to x, then apply f again to the result
    pass

double = lambda x: x * 2
add_ten = lambda x: x + 10

print(apply_twice(double, 3))
print(apply_twice(add_ten, 5))
print(apply_twice(str.upper, "hello"))
`,

	solution: `def apply_twice(f, x):
    return f(f(x))

double = lambda x: x * 2
add_ten = lambda x: x + 10

print(apply_twice(double, 3))
print(apply_twice(add_ten, 5))
print(apply_twice(str.upper, "hello"))
`,

	tests: [
		{
			name: "apply_twice(double, 3) = 12",
			code: `{{FUNC}}
double = lambda x: x * 2
print(apply_twice(double, 3))`,
			expected: "12\n",
		},
		{
			name: "apply_twice(add_ten, 5) = 25",
			code: `{{FUNC}}
add_ten = lambda x: x + 10
print(apply_twice(add_ten, 5))`,
			expected: "25\n",
		},
		{
			name: "apply_twice with str.upper (idempotent)",
			code: `{{FUNC}}
print(apply_twice(str.upper, "hello"))`,
			expected: "HELLO\n",
		},
		{
			name: "apply_twice(negate, 7) = 7",
			code: `{{FUNC}}
negate = lambda x: -x
print(apply_twice(negate, 7))`,
			expected: "7\n",
		},
	],
};
