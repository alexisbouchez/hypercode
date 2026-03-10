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

## Types as First-Class Citizens

An important concept in Lean: **types are values too**. You can assign a type to a name just like any other value:

\`\`\`lean
def MyType := Nat
\`\`\`

This is the beginning of what makes Lean a **dependently typed** language. In most languages, types and values live in separate worlds. In Lean, there is no hard boundary — types can be computed, passed to functions, and returned from functions. This seemingly simple idea is what enables Lean to serve as both a programming language and a theorem prover. We will see more of this power in later lessons.

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
