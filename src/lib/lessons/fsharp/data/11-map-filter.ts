import type { Lesson } from "../../types";

export const mapFilter: Lesson = {
  id: "map-filter",
  title: "Map & Filter",
  chapterId: "collections",
  content: `## Map & Filter

\`List.map\` transforms every element:

\`\`\`fsharp
let squares = List.map (fun x -> x * x) [1; 2; 3; 4; 5]
// [1; 4; 9; 16; 25]
\`\`\`

\`List.filter\` keeps elements that satisfy a predicate:

\`\`\`fsharp
let evens = List.filter (fun x -> x % 2 = 0) [1; 2; 3; 4; 5; 6]
// [2; 4; 6]
\`\`\`

Combine them with the pipe operator:

\`\`\`fsharp
let result =
    [1..10]
    |> List.filter (fun x -> x % 2 <> 0)
    |> List.map (fun x -> x * x)
    |> List.sum
// 1 + 9 + 25 + 49 + 81 = 165
\`\`\`

### Your Task

Write a function \`evenSquaresSum\` that takes a list of integers, keeps the even numbers, squares each, and returns the sum. Use pipes.`,

  starterCode: `let evenSquaresSum lst =
    lst
    |> List.filter (fun x -> x % 2 = 0)
    |> List.map (fun x -> x * x)
    |> List.sum

let r1 = evenSquaresSum [1; 2; 3; 4; 5; 6]
let r2 = evenSquaresSum [1..10]
printfn $"{r1}"
printfn $"{r2}"
`,

  solution: `let evenSquaresSum lst =
    lst
    |> List.filter (fun x -> x % 2 = 0)
    |> List.map (fun x -> x * x)
    |> List.sum

let r1 = evenSquaresSum [1; 2; 3; 4; 5; 6]
let r2 = evenSquaresSum [1..10]
printfn $"{r1}"
printfn $"{r2}"
`,

  tests: [
    {
      name: "evenSquaresSum [1;2;3;4;5;6] = 56",
      expected: "56\n",
      code: `{{FUNC}}
let r = evenSquaresSum [1; 2; 3; 4; 5; 6]
printfn $"{r}"
`,
    },
    {
      name: "evenSquaresSum [1..10] = 220",
      expected: "220\n",
      code: `{{FUNC}}
let r = evenSquaresSum [1..10]
printfn $"{r}"
`,
    },
  ],
};
