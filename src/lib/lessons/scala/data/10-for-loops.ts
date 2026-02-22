import type { Lesson } from "../../types";

export const forLoops: Lesson = {
  id: "for-loops",
  title: "For Loops",
  chapterId: "collections",
  content: `## For Loops

Scala's \`for\` loop iterates over ranges or collections:

\`\`\`scala
for (i <- 1 to 5) {
  println(i)
}
// 1 2 3 4 5
\`\`\`

Use \`until\` for exclusive end:

\`\`\`scala
for (i <- 0 until 3) {
  println(i)
}
// 0 1 2
\`\`\`

### Iterate over a List

\`\`\`scala
val fruits = List("apple", "banana", "cherry")
for (fruit <- fruits) {
  println(fruit)
}
\`\`\`

### Your Task

Write a function \`sumTo(n: Int): Int\` that uses a \`for\` loop and a mutable variable to sum all integers from 1 to \`n\` (inclusive).`,

  starterCode: `def sumTo(n: Int): Int = {
  var total = 0
  for (i <- 1 to n) {
    total = total + i
  }
  total
}

println(sumTo(10))
println(sumTo(5))
`,

  solution: `def sumTo(n: Int): Int = {
  var total = 0
  for (i <- 1 to n) {
    total = total + i
  }
  total
}

println(sumTo(10))
println(sumTo(5))
`,

  tests: [
    {
      name: "sumTo(10) = 55",
      expected: "55\n",
      code: `{{FUNC}}
println(sumTo(10))
`,
    },
    {
      name: "sumTo(5) = 15",
      expected: "15\n",
      code: `{{FUNC}}
println(sumTo(5))
`,
    },
    {
      name: "sumTo(1) = 1",
      expected: "1\n",
      code: `{{FUNC}}
println(sumTo(1))
`,
    },
  ],
};
