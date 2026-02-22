import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "basics",
  content: `## Hello, World!

F# uses \`printfn\` to print to the console. The \`fn\` stands for "with newline":

\`\`\`fsharp
printfn "Hello, World!"
\`\`\`

F# is a functional-first language on the .NET platform. It is statically typed, concise, and designed for both data manipulation and general-purpose programming.

### Your Task

Print \`Hello, World!\` to the console.`,

  starterCode: `printfn "Hello, World!"
`,

  solution: `printfn "Hello, World!"
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
