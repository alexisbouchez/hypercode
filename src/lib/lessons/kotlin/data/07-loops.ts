import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## Loops in Kotlin

### for loops

Iterate over a range with \`..\`:

\`\`\`kotlin
for (i in 1..5) {
    println(i)   // prints 1, 2, 3, 4, 5
}
\`\`\`

Use \`until\` to exclude the upper bound:

\`\`\`kotlin
for (i in 0 until 5) {
    println(i)   // prints 0, 1, 2, 3, 4
}
\`\`\`

Iterate over a collection:

\`\`\`kotlin
val fruits = listOf("apple", "banana", "cherry")
for (fruit in fruits) {
    println(fruit)
}
\`\`\`

### while loops

\`\`\`kotlin
var n = 1
while (n <= 3) {
    println(n)
    n += 1
}
\`\`\`

## Your Turn

Use a \`for\` loop over the range \`1..5\` to compute and print the sum of squares (1 + 4 + 9 + 16 + 25 = 55).
`,
  starterCode: `fun main() {
    var sum = 0
    for (i in 1..5) {
        sum += i * i
    }
    println(sum)
}
`,
  solution: `fun main() {
    var sum = 0
    for (i in 1..5) {
        sum += i * i
    }
    println(sum)
}
`,
  tests: [
    {
      name: "sum of squares 1..5 = 55",
      expected: "55\n",
    },
  ],
};
