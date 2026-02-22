import type { Lesson } from "../../types";

export const pipe: Lesson = {
  id: "pipe",
  title: "Pipe Operator",
  chapterId: "functions",
  content: `## Pipe Operator

The \`|>\` operator passes a value as the last argument to a function. It lets you write left-to-right data pipelines:

\`\`\`fsharp
let result =
    [1; 2; 3; 4; 5]
    |> List.filter (fun x -> x % 2 = 0)
    |> List.map (fun x -> x * x)
    |> List.sum
printfn $"{result}"  // 20  (4 + 16)
\`\`\`

This reads top-to-bottom: start with the list, keep evens, square each, sum all.

Without pipes, the same code would be deeply nested:
\`\`\`fsharp
let result = List.sum (List.map (fun x -> x * x) (List.filter (fun x -> x % 2 = 0) [1;2;3;4;5]))
\`\`\`

### Your Task

Write a pipeline that takes the list \`[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]\`, keeps only numbers divisible by 3, doubles each, and sums the result. Print the answer.`,

  starterCode: `let result =
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    |> List.filter (fun x -> x % 3 = 0)
    |> List.map (fun x -> x * 2)
    |> List.sum

printfn $"{result}"
`,

  solution: `let result =
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    |> List.filter (fun x -> x % 3 = 0)
    |> List.map (fun x -> x * 2)
    |> List.sum

printfn $"{result}"
`,

  tests: [
    {
      name: "sum of doubled multiples of 3 from 1..10 = 36",
      expected: "36\n",
    },
  ],
};
