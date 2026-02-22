import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions

Functions in Scala are defined with \`def\`. The return type comes after \`:\` and the body after \`=\`:

\`\`\`scala
def add(a: Int, b: Int): Int = a + b
println(add(3, 4))  // 7
\`\`\`

For multi-statement bodies, use curly braces:

\`\`\`scala
def greet(name: String): String = {
  val message = "Hello, " + name
  message + "!"
}
\`\`\`

### Default Parameters

Functions can have default values:

\`\`\`scala
def power(base: Int, exp: Int = 2): Int = {
  var result = 1
  for (i <- 1 to exp) result *= base
  return result
}
power(3)     // 9  (uses default exp=2)
power(2, 10) // 1024
\`\`\`

### Your Task

Write a function \`area\` that takes \`width\` and \`height\` (both \`Int\`) and returns their product.`,

  starterCode: `def area(width: Int, height: Int): Int = width * height

println(area(5, 4))
println(area(3, 7))
`,

  solution: `def area(width: Int, height: Int): Int = width * height

println(area(5, 4))
println(area(3, 7))
`,

  tests: [
    {
      name: "area 5x4",
      expected: "20\n",
      code: `{{FUNC}}
println(area(5, 4))
`,
    },
    {
      name: "area 3x7",
      expected: "21\n",
      code: `{{FUNC}}
println(area(3, 7))
`,
    },
  ],
};
