import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "basics",
  content: `## Strings in Kotlin

Kotlin strings support **string templates** — embed variables or expressions directly inside a string using \`$\`:

\`\`\`kotlin
val name = "World"
println("Hello, $name!")           // Hello, World!
println("Length: \${name.length}") // Length: 5
\`\`\`

Use \`$identifier\` for simple variables and \`\${expression}\` for any expression.

Common string methods:

| Method | Description |
|--------|-------------|
| \`.length\` | Number of characters |
| \`.uppercase()\` | Convert to uppercase |
| \`.lowercase()\` | Convert to lowercase |
| \`.contains("x")\` | Check if string contains substring |
| \`.trim()\` | Remove leading/trailing whitespace |

## Your Turn

Given \`val city = "Berlin"\`, print the city name, its length, and its uppercase version.
`,
  starterCode: `fun main() {
    val city = "Berlin"
    println(city)
    println("Length: \${city.length}")
    println(city.uppercase())
}
`,
  solution: `fun main() {
    val city = "Berlin"
    println(city)
    println("Length: \${city.length}")
    println(city.uppercase())
}
`,
  tests: [
    {
      name: "prints city info",
      expected: "Berlin\nLength: 6\nBERLIN\n",
    },
  ],
};
