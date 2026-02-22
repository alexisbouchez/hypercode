import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "basics",
  content: `## Conditionals

In F#, \`if/elif/else\` is an **expression** — it returns a value:

\`\`\`fsharp
let score = 85
let grade = if score >= 90 then "A" elif score >= 80 then "B" else "C"
printfn $"{grade}"  // B
\`\`\`

Use it directly as a function body:

\`\`\`fsharp
let max a b = if a > b then a else b
\`\`\`

### Comparison & Logic

\`=\`, \`<>\`, \`<\`, \`>\`, \`<=\`, \`>=\`, \`&&\`, \`||\`, \`not\`

Note: F# uses \`=\` for equality (not \`==\`) and \`<>\` for inequality.

### Your Task

Write a function \`sign\` that takes an integer and returns:
- \`"positive"\` if greater than 0
- \`"negative"\` if less than 0
- \`"zero"\` if equal to 0`,

  starterCode: `let sign n = if n > 0 then "positive" elif n < 0 then "negative" else "zero"

let r1 = sign 5
let r2 = sign -3
let r3 = sign 0
printfn $"{r1}"
printfn $"{r2}"
printfn $"{r3}"
`,

  solution: `let sign n = if n > 0 then "positive" elif n < 0 then "negative" else "zero"

let r1 = sign 5
let r2 = sign -3
let r3 = sign 0
printfn $"{r1}"
printfn $"{r2}"
printfn $"{r3}"
`,

  tests: [
    {
      name: "positive number",
      expected: "positive\n",
      code: `{{FUNC}}
let r = sign 5
printfn $"{r}"
`,
    },
    {
      name: "negative number",
      expected: "negative\n",
      code: `{{FUNC}}
let r = sign -3
printfn $"{r}"
`,
    },
    {
      name: "zero",
      expected: "zero\n",
      code: `{{FUNC}}
let r = sign 0
printfn $"{r}"
`,
    },
  ],
};
