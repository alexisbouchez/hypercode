import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "If Expressions",
  chapterId: "basics",
  content: `## If Expressions

In Scala, \`if/else\` is an expression — it returns a value:

\`\`\`scala
val score = 85
val grade = if (score >= 90) "A" else if (score >= 80) "B" else "C"
println(grade)  // B
\`\`\`

This means you can use \`if/else\` directly as a function body:

\`\`\`scala
def max(a: Int, b: Int): Int = if (a > b) a else b
\`\`\`

### Comparison Operators

\`==\`, \`!=\`, \`<\`, \`>\`, \`<=\`, \`>=\`

### Logical Operators

\`&&\` (and), \`||\` (or), \`!\` (not)

### Your Task

Write a function \`sign\` that takes an \`Int\` and returns:
- \`"positive"\` if greater than 0
- \`"negative"\` if less than 0
- \`"zero"\` if equal to 0`,

  starterCode: `def sign(n: Int): String = if (n > 0) "positive" else if (n < 0) "negative" else "zero"

println(sign(5))
println(sign(-3))
println(sign(0))
`,

  solution: `def sign(n: Int): String = if (n > 0) "positive" else if (n < 0) "negative" else "zero"

println(sign(5))
println(sign(-3))
println(sign(0))
`,

  tests: [
    {
      name: "positive number",
      expected: "positive\n",
      code: `{{FUNC}}
println(sign(5))
`,
    },
    {
      name: "negative number",
      expected: "negative\n",
      code: `{{FUNC}}
println(sign(-3))
`,
    },
    {
      name: "zero",
      expected: "zero\n",
      code: `{{FUNC}}
println(sign(0))
`,
    },
  ],
};
