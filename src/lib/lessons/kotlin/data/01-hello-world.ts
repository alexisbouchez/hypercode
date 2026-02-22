import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "basics",
  content: `## Your First Kotlin Program

Kotlin programs start with a \`main\` function — the entry point where execution begins.

\`\`\`kotlin
fun main() {
    println("Hello, World!")
}
\`\`\`

- \`fun\` declares a function
- \`main\` is the required entry point
- \`println\` prints a line to standard output (with a newline at the end)
- \`print\` prints without a trailing newline

## Your Turn

Print the message \`Hello, Kotlin!\` to the output.
`,
  starterCode: `fun main() {
    println("Hello, Kotlin!")
}
`,
  solution: `fun main() {
    println("Hello, Kotlin!")
}
`,
  tests: [
    {
      name: 'prints "Hello, Kotlin!"',
      expected: "Hello, Kotlin!\n",
    },
  ],
};
