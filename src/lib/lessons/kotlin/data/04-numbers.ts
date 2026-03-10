import type { Lesson } from "../../types";

export const numbers: Lesson = {
  id: "numbers",
  title: "Numbers",
  chapterId: "basics",
  content: `## Numbers in Kotlin

Kotlin's two main numeric types are \`Int\` (whole numbers) and \`Double\` (decimals).

\`\`\`kotlin
val x = 10       // Int
val pi = 3.14    // Double
\`\`\`

Arithmetic operators work as expected:

\`\`\`kotlin
println(10 + 3)   // 13
println(10 - 3)   // 7
println(10 * 3)   // 30
println(10 % 3)   // 1  (remainder)
\`\`\`

Useful math functions:

\`\`\`kotlin
kotlin.math.sqrt(16.0)  // 4.0
kotlin.math.abs(-5)     // 5
minOf(3, 7)             // 3
maxOf(3, 7)             // 7
\`\`\`

## Your Turn

Compute and print: the sum, difference, product, and remainder when dividing 15 by 4.
`,
  starterCode: `fun main() {
    val a = 15
    val b = 4
    println(a + b)
    println(a - b)
    println(a * b)
    println(a % b)
}
`,
  solution: `fun main() {
    val a = 15
    val b = 4
    println(a + b)
    println(a - b)
    println(a * b)
    println(a % b)
}
`,
  tests: [
    {
      name: "arithmetic on 15 and 4",
      expected: "19\n11\n60\n3\n",
    },
    {
      name: "minOf and maxOf",
      code: "fun main() {\n    println(minOf(3, 7))\n    println(maxOf(3, 7))\n}",
      expected: "3\n7\n",
    },
    {
      name: "arithmetic with different operands",
      code: "fun main() {\n    val a = 10\n    val b = 3\n    println(a + b)\n    println(a * b)\n    println(a % b)\n}",
      expected: "13\n30\n1\n",
    },
  ],
};
