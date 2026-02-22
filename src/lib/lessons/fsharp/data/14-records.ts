import type { Lesson } from "../../types";

export const records: Lesson = {
  id: "records",
  title: "Records",
  chapterId: "types",
  content: `## Records

Records are named, immutable data structures with labeled fields:

\`\`\`fsharp
type Point = { X: int; Y: int }

let p = { X = 3; Y = 4 }
printfn $"({p.X}, {p.Y})"
\`\`\`

Create a modified copy with \`with\`:

\`\`\`fsharp
let p2 = { p with X = 10 }
// { X = 10; Y = 4 }
\`\`\`

Records work well as function parameters:

\`\`\`fsharp
let distance p = sqrt (float (p.X * p.X + p.Y * p.Y))
\`\`\`

### Your Task

Define a record type \`Rectangle\` with fields \`Width: int\` and \`Height: int\`. Then write a function \`area\` that returns \`Width * Height\`, and a function \`perimeter\` that returns \`2 * (Width + Height)\`.`,

  starterCode: `type Rectangle = { Width: int; Height: int }

let area r = r.Width * r.Height
let perimeter r = 2 * (r.Width + r.Height)

let rect = { Width = 5; Height = 3 }
printfn $"{area rect}"
printfn $"{perimeter rect}"
`,

  solution: `type Rectangle = { Width: int; Height: int }

let area r = r.Width * r.Height
let perimeter r = 2 * (r.Width + r.Height)

let rect = { Width = 5; Height = 3 }
printfn $"{area rect}"
printfn $"{perimeter rect}"
`,

  tests: [
    {
      name: "area of 5x3 = 15",
      expected: "15\n",
      code: `{{FUNC}}
let r = { Width = 5; Height = 3 }
printfn $"{area r}"
`,
    },
    {
      name: "perimeter of 5x3 = 16",
      expected: "16\n",
      code: `{{FUNC}}
let r = { Width = 5; Height = 3 }
printfn $"{perimeter r}"
`,
    },
    {
      name: "area of 7x4 = 28",
      expected: "28\n",
      code: `{{FUNC}}
let r = { Width = 7; Height = 4 }
printfn $"{area r}"
`,
    },
  ],
};
