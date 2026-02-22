import type { Lesson } from "../../types";

export const recursion: Lesson = {
  id: "recursion",
  title: "Recursion",
  chapterId: "functions",
  content: `## Recursion

F# supports recursion with \`let rec\`:

\`\`\`fsharp
let rec factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)

printfn $"{factorial 5}"  // 120
\`\`\`

The \`rec\` keyword is required — F# doesn't allow a function to call itself without it.

### Your Task

Write a recursive function \`power\` that takes a base \`b\` and exponent \`e\` (both integers) and returns \`b\` raised to the power \`e\`. Assume \`e >= 0\`. Base case: \`power b 0 = 1\`.`,

  starterCode: `let rec power b e =
    if e = 0 then 1
    else b * (power b (e - 1))

let r1 = power 2 10
let r2 = power 3 4
printfn $"{r1}"
printfn $"{r2}"
`,

  solution: `let rec power b e =
    if e = 0 then 1
    else b * (power b (e - 1))

let r1 = power 2 10
let r2 = power 3 4
printfn $"{r1}"
printfn $"{r2}"
`,

  tests: [
    {
      name: "2^10 = 1024",
      expected: "1024\n",
      code: `{{FUNC}}
let r = power 2 10
printfn $"{r}"
`,
    },
    {
      name: "3^4 = 81",
      expected: "81\n",
      code: `{{FUNC}}
let r = power 3 4
printfn $"{r}"
`,
    },
    {
      name: "5^0 = 1",
      expected: "1\n",
      code: `{{FUNC}}
let r = power 5 0
printfn $"{r}"
`,
    },
  ],
};
