import type { Lesson } from "../../types";

export const stringInterpolation: Lesson = {
  id: "string-interpolation",
  title: "String Interpolation",
  chapterId: "basics",
  content: `## String Interpolation

Scala's \`s"..."\` strings let you embed values with \`$name\` or \`\${expr}\`:

\`\`\`scala
val name = "Alice"
val age = 30
println(s"Hello, $name! You are $age years old.")
// Hello, Alice! You are 30 years old.
\`\`\`

For arbitrary expressions, use curly braces:

\`\`\`scala
val a = 3
val b = 4
println(s"\$a + \$b = \${a + b}")
// 3 + 4 = 7
\`\`\`

### Your Task

Write a function \`greet\` that takes a name (String) and an age (Int) and returns a greeting string using string interpolation: \`"Hello, <name>! You are <age> years old."\``,

  starterCode: `def greet(name: String, age: Int): String = s"Hello, $name! You are $age years old."

println(greet("Alice", 30))
`,

  solution: `def greet(name: String, age: Int): String = s"Hello, $name! You are $age years old."

println(greet("Alice", 30))
`,

  tests: [
    {
      name: "greet Alice 30",
      expected: "Hello, Alice! You are 30 years old.\n",
      code: `{{FUNC}}
println(greet("Alice", 30))
`,
    },
    {
      name: "greet Bob 25",
      expected: "Hello, Bob! You are 25 years old.\n",
      code: `{{FUNC}}
println(greet("Bob", 25))
`,
    },
  ],
};
