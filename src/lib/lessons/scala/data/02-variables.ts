import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Values & Variables",
  chapterId: "basics",
  content: `## Values and Variables

Scala distinguishes between immutable values (\`val\`) and mutable variables (\`var\`):

\`\`\`scala
val pi = 3.14159    // immutable — cannot be reassigned
var count = 0       // mutable — can be reassigned
count = 10
\`\`\`

Scala infers types automatically. You can also annotate them explicitly:

\`\`\`scala
val name: String = "Alice"
var age: Int = 30
\`\`\`

### Basic Types

| Type | Example |
|------|---------|
| \`Int\` | \`42\` |
| \`Double\` | \`3.14\` |
| \`String\` | \`"hello"\` |
| \`Boolean\` | \`true\`, \`false\` |

### Your Task

Create a \`val x = 10\`, a \`var y = 5\`, then reassign \`y\` to \`y + x\`, and print \`y\`.`,

  starterCode: `val x = 10
var y = 5
y = y + x
println(y)
`,

  solution: `val x = 10
var y = 5
y = y + x
println(y)
`,

  tests: [
    {
      name: "prints 15",
      expected: "15\n",
    },
  ],
};
