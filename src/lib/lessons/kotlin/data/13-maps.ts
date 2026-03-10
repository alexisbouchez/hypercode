import type { Lesson } from "../../types";

export const maps: Lesson = {
  id: "maps",
  title: "Maps",
  chapterId: "collections",
  content: `## Maps in Kotlin

A \`Map\` stores key-value pairs. Create one with \`mapOf\`:

\`\`\`kotlin
val scores = mapOf("Alice" to 95, "Bob" to 87)
\`\`\`

Access values by key using square brackets:

\`\`\`kotlin
println(scores["Alice"])  // 95
println(scores["Bob"])    // 87
\`\`\`

Check the number of entries:

\`\`\`kotlin
println(scores.size)  // 2
\`\`\`

Check for a key:

\`\`\`kotlin
println(scores.contains("Alice"))  // true
\`\`\`

## Your Turn

Create a map of three capitals: \`"France"\` → \`"Paris"\`, \`"Japan"\` → \`"Tokyo"\`, \`"Brazil"\` → \`"Brasília"\`. Print the capital of France, the capital of Japan, and the map's size.
`,
  starterCode: `fun main() {
    val capitals = mapOf("France" to "Paris", "Japan" to "Tokyo", "Brazil" to "Brasília")
    println(capitals["France"])
    println(capitals["Japan"])
    println(capitals.size)
}
`,
  solution: `fun main() {
    val capitals = mapOf("France" to "Paris", "Japan" to "Tokyo", "Brazil" to "Brasília")
    println(capitals["France"])
    println(capitals["Japan"])
    println(capitals.size)
}
`,
  tests: [
    {
      name: "capitals map",
      expected: "Paris\nTokyo\n3\n",
    },
    {
      name: "map key access and size",
      code: 'fun main() {\n    val m = mapOf("x" to 10, "y" to 20, "z" to 30)\n    println(m["x"])\n    println(m["z"])\n    println(m.size)\n}',
      expected: "10\n30\n3\n",
    },
    {
      name: "map size and value access",
      code: 'fun main() {\n    val scores = mapOf("Alice" to 95, "Bob" to 87)\n    println(scores.size)\n    println(scores["Alice"])\n}',
      expected: "2\n95\n",
    },
  ],
};
