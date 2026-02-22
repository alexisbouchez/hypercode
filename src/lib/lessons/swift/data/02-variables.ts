import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables & Constants",
  chapterId: "basics",
  content: `## Variables and Constants

Swift uses \`let\` for constants and \`var\` for variables:

\`\`\`swift
let pi = 3.14159     // constant — cannot be changed
var count = 0        // variable — can be changed
count = 10
\`\`\`

Swift infers the type from the initial value. You can also annotate explicitly:

\`\`\`swift
let name: String = "Alice"
var age: Int = 30
\`\`\`

### Basic Types

| Type | Example |
|------|---------|
| \`Int\` | \`42\` |
| \`Double\` | \`3.14\` |
| \`String\` | \`"hello"\` |
| \`Bool\` | \`true\`, \`false\` |

### Your Task

Create a constant \`x\` with value \`10\`, a variable \`y\` with value \`5\`, then set \`y\` to \`y + x\` and print \`y\`.`,

  starterCode: `let x = 10
var y = 5
y = y + x
print(y)
`,

  solution: `let x = 10
var y = 5
y = y + x
print(y)
`,

  tests: [
    {
      name: "prints 15",
      expected: "15\n",
    },
  ],
};
