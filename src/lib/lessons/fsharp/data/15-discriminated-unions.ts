import type { Lesson } from "../../types";

export const discriminatedUnions: Lesson = {
  id: "discriminated-unions",
  title: "Discriminated Unions",
  chapterId: "types",
  content: `## Discriminated Unions

Discriminated Unions (DUs) let you define types with named cases, each possibly carrying data:

\`\`\`fsharp
type Shape =
    | Circle of float
    | Rectangle of float * float

let area shape = match shape with
                 | Circle r -> 3.14159 * r * r
                 | Rectangle (w, h) -> w * h
\`\`\`

DUs are ideal for modeling alternatives — each case is a distinct constructor.

Use \`match\` to pattern-match on the case and extract the data.

### Your Task

Define a DU \`Expr\` with three cases:
- \`Num of int\` — a literal number
- \`Add of int * int\` — addition of two numbers
- \`Mul of int * int\` — multiplication of two numbers

Write a function \`eval\` that pattern-matches on an \`Expr\` and returns an \`int\`.`,

  starterCode: `type Expr =
    | Num of int
    | Add of int * int
    | Mul of int * int

let eval expr =
    match expr with
    | Num n -> n
    | Add (a, b) -> a + b
    | Mul (a, b) -> a * b

printfn $"{eval (Num 42)}"
printfn $"{eval (Add (3, 4))}"
printfn $"{eval (Mul (6, 7))}"
`,

  solution: `type Expr =
    | Num of int
    | Add of int * int
    | Mul of int * int

let eval expr =
    match expr with
    | Num n -> n
    | Add (a, b) -> a + b
    | Mul (a, b) -> a * b

printfn $"{eval (Num 42)}"
printfn $"{eval (Add (3, 4))}"
printfn $"{eval (Mul (6, 7))}"
`,

  tests: [
    {
      name: "Num 42 = 42",
      expected: "42\n",
      code: `{{FUNC}}
printfn $"{eval (Num 42)}"
`,
    },
    {
      name: "Add (3, 4) = 7",
      expected: "7\n",
      code: `{{FUNC}}
printfn $"{eval (Add (3, 4))}"
`,
    },
    {
      name: "Mul (6, 7) = 42",
      expected: "42\n",
      code: `{{FUNC}}
printfn $"{eval (Mul (6, 7))}"
`,
    },
  ],
};
