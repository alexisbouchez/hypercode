import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables",
  chapterId: "foundations",
  content: `## Defining Names in Lean

In Lean, you define top-level names with \`def\`. These are immutable bindings (not mutable variables):

\`\`\`lean
def x : Nat := 42
def greeting : String := "Hello, Lean!"
def isPure : Bool := true

#eval x           -- 42
#eval greeting    -- "Hello, Lean!"
#eval isPure      -- true
\`\`\`

The type annotation (\`: Nat\`, \`: String\`) is optional — Lean can infer types. But writing them makes code clearer.

You can also define names without type annotations:

\`\`\`lean
def answer := 42
\`\`\`

## Your Turn

Define three constants:
- \`name\` as the string \`"Lean"\`
- \`version\` as the number \`4\`
- \`isFunctional\` as \`true\`

Then evaluate all three.
`,
  starterCode: `def name : String := "Lean"
def version : Nat := 4
def isFunctional : Bool := true

#eval name
#eval version
#eval isFunctional
`,
  solution: `def name : String := "Lean"
def version : Nat := 4
def isFunctional : Bool := true

#eval name
#eval version
#eval isFunctional
`,
  tests: [
    {
      name: "name, version, isFunctional",
      expected: '"Lean"\n4\ntrue\n',
    },
  ],
};
