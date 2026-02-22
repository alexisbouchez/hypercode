import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables",
  chapterId: "basics",
  content: `## Variables in Kotlin

Kotlin has two kinds of variables:

- \`val\` — immutable (read-only), like \`const\` in other languages
- \`var\` — mutable, can be reassigned

\`\`\`kotlin
val name = "Alice"   // cannot be changed
var score = 0        // can be changed
score = 100
\`\`\`

Kotlin infers types automatically, so you rarely need to write them explicitly:

\`\`\`kotlin
val age: Int = 25    // explicit type
val city = "Berlin"  // inferred as String
\`\`\`

## Your Turn

Create a \`val\` named \`language\` with the value \`"Kotlin"\`, then create a \`var\` named \`version\` starting at \`1\`, reassign it to \`2\`, and print both.
`,
  starterCode: `fun main() {
    val language = "Kotlin"
    var version = 1
    version = 2
    println(language)
    println(version)
}
`,
  solution: `fun main() {
    val language = "Kotlin"
    var version = 1
    version = 2
    println(language)
    println(version)
}
`,
  tests: [
    {
      name: "prints language and version",
      expected: "Kotlin\n2\n",
    },
  ],
};
