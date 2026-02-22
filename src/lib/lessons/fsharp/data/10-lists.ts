import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "collections",
  content: `## Lists

F# lists are immutable, singly-linked sequences written with \`[\` and \`]\`, elements separated by \`;\`:

\`\`\`fsharp
let nums = [1; 2; 3; 4; 5]
\`\`\`

You can use ranges:

\`\`\`fsharp
let oneToTen = [1..10]
\`\`\`

Common \`List\` functions:

\`\`\`fsharp
List.length [1; 2; 3]       // 3
List.head [10; 20; 30]      // 10
List.tail [10; 20; 30]      // [20; 30]
List.sum [1; 2; 3; 4; 5]    // 15
List.rev [1; 2; 3]          // [3; 2; 1]
\`\`\`

### Your Task

Create a list \`nums\` containing \`[10; 20; 30; 40; 50]\`. Print its length, its first element, and its sum on separate lines.`,

  starterCode: `let nums = [10; 20; 30; 40; 50]

printfn $"{List.length nums}"
printfn $"{List.head nums}"
printfn $"{List.sum nums}"
`,

  solution: `let nums = [10; 20; 30; 40; 50]

printfn $"{List.length nums}"
printfn $"{List.head nums}"
printfn $"{List.sum nums}"
`,

  tests: [
    {
      name: "length = 5, head = 10, sum = 150",
      expected: "5\n10\n150\n",
    },
  ],
};
