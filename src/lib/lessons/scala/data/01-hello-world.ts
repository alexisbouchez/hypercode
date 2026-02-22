import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "basics",
  content: `## Your First Scala Program

In Scala, \`println\` prints a value followed by a newline:

\`\`\`scala
println("Hello, World!")
\`\`\`

You can print multiple things by calling \`println\` multiple times, or concatenate with \`+\`:

\`\`\`scala
println("Hello, " + "Scala!")
println(42)
println(true)
\`\`\`

### Comments

\`\`\`scala
// Single-line comment
/* Multi-line
   comment */
\`\`\`

### Your Task

Print exactly \`Hello, World!\`.`,

  starterCode: `println("Hello, World!")
`,

  solution: `println("Hello, World!")
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
