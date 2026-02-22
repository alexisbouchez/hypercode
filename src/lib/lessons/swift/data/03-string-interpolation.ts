import type { Lesson } from "../../types";

export const stringInterpolation: Lesson = {
  id: "string-interpolation",
  title: "String Interpolation",
  chapterId: "basics",
  content: `## String Interpolation

Swift lets you embed values directly inside strings using \`\\(expr)\`:

\`\`\`swift
let name = "Alice"
let age = 30
print("Hello, \\(name)! You are \\(age) years old.")
// Hello, Alice! You are 30 years old.
\`\`\`

Any expression works inside the parentheses:

\`\`\`swift
let a = 3
let b = 4
print("\\(a) + \\(b) = \\(a + b)")
// 3 + 4 = 7
\`\`\`

### Your Task

Write a function \`greet\` that takes a name (String) and an age (Int), and returns the greeting string \`"Hello, <name>! You are <age> years old."\` using string interpolation. Then call it and print the result.`,

  starterCode: `func greet(_ name: String, _ age: Int) -> String {
    return "Hello, \\(name)! You are \\(age) years old."
}

print(greet("Alice", 30))
`,

  solution: `func greet(_ name: String, _ age: Int) -> String {
    return "Hello, \\(name)! You are \\(age) years old."
}

print(greet("Alice", 30))
`,

  tests: [
    {
      name: "greet Alice 30",
      expected: "Hello, Alice! You are 30 years old.\n",
      code: `{{FUNC}}
print(greet("Alice", 30))
`,
    },
    {
      name: "greet Bob 25",
      expected: "Hello, Bob! You are 25 years old.\n",
      code: `{{FUNC}}
print(greet("Bob", 25))
`,
    },
  ],
};
