import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "basics",
  content: `## Pattern Matching

F#'s \`match\` expression is more powerful than a switch. It returns a value and handles multiple patterns cleanly:

\`\`\`fsharp
let n = 3
let name = match n with
           | 1 -> "one"
           | 2 -> "two"
           | 3 -> "three"
           | _ -> "other"  // wildcard: matches anything
printfn $"{name}"  // three
\`\`\`

### OR Patterns

Use \`|\` to match multiple values in one case:

\`\`\`fsharp
let isWeekend day = match day with
                    | 6 | 7 -> true
                    | _     -> false
\`\`\`

### Your Task

Write a function \`season\` that maps a month number (1–12) to its season:
- \`"winter"\` for months 12, 1, 2
- \`"spring"\` for months 3, 4, 5
- \`"summer"\` for months 6, 7, 8
- \`"autumn"\` for months 9, 10, 11`,

  starterCode: `let season month =
    match month with
    | 12 | 1 | 2 -> "winter"
    | 3 | 4 | 5  -> "spring"
    | 6 | 7 | 8  -> "summer"
    | _          -> "autumn"

let s = season 1
printfn $"{s}"
`,

  solution: `let season month =
    match month with
    | 12 | 1 | 2 -> "winter"
    | 3 | 4 | 5  -> "spring"
    | 6 | 7 | 8  -> "summer"
    | _          -> "autumn"

let s = season 1
printfn $"{s}"
`,

  tests: [
    {
      name: "January is winter",
      expected: "winter\n",
      code: `{{FUNC}}
let r = season 1
printfn $"{r}"
`,
    },
    {
      name: "April is spring",
      expected: "spring\n",
      code: `{{FUNC}}
let r = season 4
printfn $"{r}"
`,
    },
    {
      name: "July is summer",
      expected: "summer\n",
      code: `{{FUNC}}
let r = season 7
printfn $"{r}"
`,
    },
    {
      name: "October is autumn",
      expected: "autumn\n",
      code: `{{FUNC}}
let r = season 10
printfn $"{r}"
`,
    },
  ],
};
