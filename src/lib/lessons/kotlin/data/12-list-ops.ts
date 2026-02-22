import type { Lesson } from "../../types";

export const listOps: Lesson = {
  id: "list-ops",
  title: "List Operations",
  chapterId: "collections",
  content: `## Transforming Lists

Kotlin lists have powerful higher-order methods:

### map — transform each element
\`\`\`kotlin
val nums = listOf(1, 2, 3, 4, 5)
val doubled = nums.map { it * 2 }  // [2, 4, 6, 8, 10]
\`\`\`

### filter — keep elements that match
\`\`\`kotlin
val evens = nums.filter { it % 2 == 0 }  // [2, 4]
\`\`\`

### fold — accumulate into a single value
\`\`\`kotlin
val sum = nums.fold(0) { acc, x -> acc + x }  // 15
\`\`\`

### any / all
\`\`\`kotlin
nums.any { it > 4 }   // true
nums.all { it > 0 }   // true
\`\`\`

## Your Turn

Given the list \`[1, 2, 3, 4, 5, 6]\`, print the list of squares, then the list of odd numbers, then the sum of all elements.
`,
  starterCode: `fun main() {
    val nums = listOf(1, 2, 3, 4, 5, 6)
    val squares = nums.map { it * it }
    println(squares)
    val odds = nums.filter { it % 2 != 0 }
    println(odds)
    val total = nums.fold(0) { acc, x -> acc + x }
    println(total)
}
`,
  solution: `fun main() {
    val nums = listOf(1, 2, 3, 4, 5, 6)
    val squares = nums.map { it * it }
    println(squares)
    val odds = nums.filter { it % 2 != 0 }
    println(odds)
    val total = nums.fold(0) { acc, x -> acc + x }
    println(total)
}
`,
  tests: [
    {
      name: "squares, odds, and sum",
      expected: "[1, 4, 9, 16, 25, 36]\n[1, 3, 5]\n21\n",
    },
  ],
};
