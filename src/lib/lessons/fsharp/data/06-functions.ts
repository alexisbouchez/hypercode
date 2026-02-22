import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions

F# functions are defined with \`let\`. Parameters are space-separated — no parentheses or commas needed:

\`\`\`fsharp
let add x y = x + y
let result = add 3 4
printfn $"{result}"  // 7
\`\`\`

All F# functions are **curried** by default — applying fewer arguments than needed returns a new function:

\`\`\`fsharp
let addFive = add 5   // partial application
printfn $"{addFive 3}"  // 8
\`\`\`

### Type Annotations (optional)

F# infers types, but you can be explicit:

\`\`\`fsharp
let multiply (x: int) (y: int) : int = x * y
\`\`\`

### Your Task

Write a function \`area\` that takes \`width\` and \`height\` (both integers) and returns their product.`,

  starterCode: `let area width height = width * height

let r1 = area 5 4
let r2 = area 3 7
printfn $"{r1}"
printfn $"{r2}"
`,

  solution: `let area width height = width * height

let r1 = area 5 4
let r2 = area 3 7
printfn $"{r1}"
printfn $"{r2}"
`,

  tests: [
    {
      name: "area 5x4 = 20",
      expected: "20\n",
      code: `{{FUNC}}
let r = area 5 4
printfn $"{r}"
`,
    },
    {
      name: "area 3x7 = 21",
      expected: "21\n",
      code: `{{FUNC}}
let r = area 3 7
printfn $"{r}"
`,
    },
  ],
};
