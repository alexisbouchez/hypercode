import type { Lesson } from "../../types";

export const fold: Lesson = {
  id: "fold",
  title: "Fold",
  chapterId: "collections",
  content: `## Fold

\`List.fold\` reduces a list to a single value by accumulating results:

\`\`\`fsharp
List.fold (fun acc x -> acc + x) 0 [1; 2; 3; 4; 5]
// 15
\`\`\`

The signature: \`fold (acc -> elem -> acc) initialValue list\`.

- The function takes the current accumulator and the current element
- Returns the new accumulator
- Starts with \`initialValue\`

Build a string from a list:

\`\`\`fsharp
let joined = List.fold (fun acc x -> acc + x + " ") "" ["hello"; "world"]
// "hello world "
\`\`\`

### Your Task

Write a function \`product\` that takes a list of integers and returns their product using \`List.fold\`. The identity element for multiplication is \`1\`.`,

  starterCode: `let product lst = List.fold (fun acc x -> acc * x) 1 lst

let r1 = product [1; 2; 3; 4; 5]
let r2 = product [2; 3; 4]
printfn $"{r1}"
printfn $"{r2}"
`,

  solution: `let product lst = List.fold (fun acc x -> acc * x) 1 lst

let r1 = product [1; 2; 3; 4; 5]
let r2 = product [2; 3; 4]
printfn $"{r1}"
printfn $"{r2}"
`,

  tests: [
    {
      name: "product [1;2;3;4;5] = 120",
      expected: "120\n",
      code: `{{FUNC}}
let r = product [1; 2; 3; 4; 5]
printfn $"{r}"
`,
    },
    {
      name: "product [2;3;4] = 24",
      expected: "24\n",
      code: `{{FUNC}}
let r = product [2; 3; 4]
printfn $"{r}"
`,
    },
    {
      name: "product [7] = 7",
      expected: "7\n",
      code: `{{FUNC}}
let r = product [7]
printfn $"{r}"
`,
    },
  ],
};
