import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "collections",
  content: `## Lists in Kotlin

Create an immutable list with \`listOf\`:

\`\`\`kotlin
val nums = listOf(1, 2, 3, 4, 5)
\`\`\`

Access elements by index (0-based), check size, and test membership:

\`\`\`kotlin
println(nums[0])          // 1
println(nums.size)        // 5
println(nums.contains(3)) // true
\`\`\`

For a mutable list that you can add/remove elements from:

\`\`\`kotlin
val items = mutableListOf("a", "b")
items.add("c")
println(items.size)  // 3
\`\`\`

## Your Turn

Create a list of the first 5 odd numbers \`[1, 3, 5, 7, 9]\`. Print the list's size, its first element, its last element, and whether it contains 7.
`,
  starterCode: `fun main() {
    val odds = listOf(1, 3, 5, 7, 9)
    println(odds.size)
    println(odds[0])
    println(odds[4])
    println(odds.contains(7))
}
`,
  solution: `fun main() {
    val odds = listOf(1, 3, 5, 7, 9)
    println(odds.size)
    println(odds[0])
    println(odds[4])
    println(odds.contains(7))
}
`,
  tests: [
    {
      name: "list operations",
      expected: "5\n1\n9\ntrue\n",
    },
  ],
};
