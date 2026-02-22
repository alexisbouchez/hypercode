import type { Lesson } from "../../types";

export const stringInterpolation: Lesson = {
  id: "string-interpolation",
  title: "String Interpolation",
  chapterId: "basics",
  content: `## String Interpolation

F#'s \`$"..."\` strings let you embed any expression with \`{expr}\`:

\`\`\`fsharp
let name = "Alice"
let age = 30
printfn $"Hello, {name}! You are {age} years old."
// Hello, Alice! You are 30 years old.
\`\`\`

You can embed arbitrary expressions:

\`\`\`fsharp
let a = 3
let b = 4
printfn $"{a} + {b} = {a + b}"
// 3 + 4 = 7
\`\`\`

### Your Task

Write a function \`greet\` that takes a \`name\` and \`age\` and returns the string \`"Hello, <name>! You are <age> years old."\` using string interpolation.`,

  starterCode: `let greet name age = $"Hello, {name}! You are {age} years old."

let msg = greet "Alice" 30
printfn $"{msg}"
`,

  solution: `let greet name age = $"Hello, {name}! You are {age} years old."

let msg = greet "Alice" 30
printfn $"{msg}"
`,

  tests: [
    {
      name: "greet Alice 30",
      expected: "Hello, Alice! You are 30 years old.\n",
      code: `{{FUNC}}
let msg = greet "Alice" 30
printfn $"{msg}"
`,
    },
    {
      name: "greet Bob 25",
      expected: "Hello, Bob! You are 25 years old.\n",
      code: `{{FUNC}}
let msg = greet "Bob" 25
printfn $"{msg}"
`,
    },
  ],
};
