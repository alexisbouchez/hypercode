import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "lists",
  content: `## Pattern Matching

Pattern matching is one of Lean's most powerful features. It lets you inspect a value and branch on its structure.

Use \`match\` to pattern match:

\`\`\`lean
def describe (n : Nat) : String :=
  match n with
  | 0 => "zero"
  | 1 => "one"
  | _ => "many"

#eval describe 0    -- "zero"
#eval describe 1    -- "one"
#eval describe 42   -- "many"
\`\`\`

The \`_\` pattern is a wildcard that matches anything.

You can also define functions with pattern matching directly:

\`\`\`lean
def isZero : Nat → Bool
  | 0 => true
  | _ => false
\`\`\`

## Your Turn

Define a function \`grade\` that takes a score (Nat) and returns a letter grade:
- \`90\` or more → \`"A"\`
- \`80\` or more → \`"B"\`
- \`70\` or more → \`"C"\`
- anything else → \`"F"\`

Use \`if-then-else\` inside a \`def\`.
`,
  starterCode: `def grade (score : Nat) : String :=
  if score >= 90 then "A"
  else if score >= 80 then "B"
  else if score >= 70 then "C"
  else "F"

#eval grade 95
#eval grade 82
#eval grade 71
#eval grade 60
`,
  solution: `def grade (score : Nat) : String :=
  if score >= 90 then "A"
  else if score >= 80 then "B"
  else if score >= 70 then "C"
  else "F"

#eval grade 95
#eval grade 82
#eval grade 71
#eval grade 60
`,
  tests: [
    {
      name: 'grade 95 = "A"',
      code: '{{FUNC}}\n#eval grade 95',
      expected: '"A"\n',
    },
    {
      name: 'grade 82 = "B"',
      code: '{{FUNC}}\n#eval grade 82',
      expected: '"B"\n',
    },
    {
      name: 'grade 71 = "C"',
      code: '{{FUNC}}\n#eval grade 71',
      expected: '"C"\n',
    },
    {
      name: 'grade 60 = "F"',
      code: '{{FUNC}}\n#eval grade 60',
      expected: '"F"\n',
    },
  ],
};
