import type { Lesson } from "../../types";

export const values: Lesson = {
  id: "values",
  title: "Values",
  chapterId: "basics",
  content: `## Values

In F#, \`let\` binds a name to a value. Bindings are **immutable** by default:

\`\`\`fsharp
let x = 42
let name = "Alice"
let pi = 3.14159
\`\`\`

Use \`let mutable\` when you need reassignment:

\`\`\`fsharp
let mutable count = 0
count <- count + 1
printfn $"{count}"  // 1
\`\`\`

### String Interpolation

F# 5+ supports \`$"..."\` for embedding values:

\`\`\`fsharp
let city = "Paris"
printfn $"I live in {city}."
\`\`\`

### Your Task

Create a \`let\` binding \`score = 100\`, a \`let mutable\` binding \`level = 1\`, increment \`level\` by 1, then print \`score\` and \`level\` on separate lines.`,

  starterCode: `let score = 100
let mutable level = 1
level <- level + 1
printfn $"{score}"
printfn $"{level}"
`,

  solution: `let score = 100
let mutable level = 1
level <- level + 1
printfn $"{score}"
printfn $"{level}"
`,

  tests: [
    {
      name: "score = 100, level = 2",
      expected: "100\n2\n",
    },
  ],
};
