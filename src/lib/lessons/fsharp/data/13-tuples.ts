import type { Lesson } from "../../types";

export const tuples: Lesson = {
  id: "tuples",
  title: "Tuples",
  chapterId: "collections",
  content: `## Tuples

Tuples group multiple values without naming them:

\`\`\`fsharp
let point = (3, 4)
let name, age = "Alice", 30  // destructuring
printfn $"{name} is {age}"
\`\`\`

Use \`fst\` and \`snd\` to access the first and second element of a pair:

\`\`\`fsharp
let p = (10, 20)
printfn $"{fst p}"  // 10
printfn $"{snd p}"  // 20
\`\`\`

Destructure in function parameters:

\`\`\`fsharp
let addPair (a, b) = a + b
printfn $"{addPair (3, 4)}"  // 7
\`\`\`

### Your Task

Write a function \`swap\` that takes a tuple \`(a, b)\` and returns \`(b, a)\`. Then write a function \`sumPair\` that takes a tuple of two integers and returns their sum.`,

  starterCode: `let swap (a, b) = (b, a)
let sumPair (a, b) = a + b

let s1 = swap (1, 2)
let s2 = sumPair (3, 7)
printfn $"{fst s1}"
printfn $"{snd s1}"
printfn $"{s2}"
`,

  solution: `let swap (a, b) = (b, a)
let sumPair (a, b) = a + b

let s1 = swap (1, 2)
let s2 = sumPair (3, 7)
printfn $"{fst s1}"
printfn $"{snd s1}"
printfn $"{s2}"
`,

  tests: [
    {
      name: "swap (1, 2) = (2, 1)",
      expected: "2\n1\n",
      code: `{{FUNC}}
let s = swap (1, 2)
printfn $"{fst s}"
printfn $"{snd s}"
`,
    },
    {
      name: "sumPair (3, 7) = 10",
      expected: "10\n",
      code: `{{FUNC}}
let r = sumPair (3, 7)
printfn $"{r}"
`,
    },
    {
      name: "swap (\"hello\", 42)",
      expected: "42\nhello\n",
      code: `{{FUNC}}
let s = swap ("hello", 42)
printfn $"{fst s}"
printfn $"{snd s}"
`,
    },
  ],
};
