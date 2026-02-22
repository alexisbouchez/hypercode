import type { Lesson } from "../../types";

export const higherOrder: Lesson = {
  id: "higher-order",
  title: "Higher-Order Functions",
  chapterId: "functions",
  content: `## Higher-Order Functions

Functions in F# are first-class values — you can pass them as arguments:

\`\`\`fsharp
let apply f x = f x

let double x = x * 2
printfn $"{apply double 5}"  // 10
\`\`\`

Anonymous functions use \`fun\`:

\`\`\`fsharp
let result = apply (fun x -> x + 10) 5
printfn $"{result}"  // 15
\`\`\`

\`List.map\` applies a function to every element:

\`\`\`fsharp
let nums = [1; 2; 3; 4; 5]
let doubled = List.map (fun x -> x * 2) nums
// [2; 4; 6; 8; 10]
\`\`\`

### Your Task

Write a function \`sumDoubled\` that takes a list of integers and returns the sum of each element doubled. Use \`List.map\` and \`List.sum\`.`,

  starterCode: `let sumDoubled lst =
    List.map (fun x -> x * 2) lst |> List.sum

let r1 = sumDoubled [1; 2; 3; 4; 5]
let r2 = sumDoubled [10; 20; 30]
printfn $"{r1}"
printfn $"{r2}"
`,

  solution: `let sumDoubled lst =
    List.map (fun x -> x * 2) lst |> List.sum

let r1 = sumDoubled [1; 2; 3; 4; 5]
let r2 = sumDoubled [10; 20; 30]
printfn $"{r1}"
printfn $"{r2}"
`,

  tests: [
    {
      name: "sumDoubled [1;2;3;4;5] = 30",
      expected: "30\n",
      code: `{{FUNC}}
let r = sumDoubled [1; 2; 3; 4; 5]
printfn $"{r}"
`,
    },
    {
      name: "sumDoubled [10;20;30] = 120",
      expected: "120\n",
      code: `{{FUNC}}
let r = sumDoubled [10; 20; 30]
printfn $"{r}"
`,
    },
  ],
};
