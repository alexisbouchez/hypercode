import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "functions",
  content: `## Pattern Matching

Haskell functions can have multiple equations, each matching a different pattern:

\`\`\`haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
\`\`\`

Patterns are tried top-to-bottom. The first matching equation is used.

### Wildcard Pattern

Use \`_\` to match anything without binding it:

\`\`\`haskell
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False
\`\`\`

### List Patterns

You can pattern match on list structure with \`[]\` (empty) and \`(x:xs)\` (head:tail):

\`\`\`haskell
myHead :: [a] -> a
myHead (x:_) = x
\`\`\`

### Your Task

Define \`factorial\` using pattern matching (base case \`0 = 1\`, recursive case). Print \`factorial 10\`.`,

  starterCode: `factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = print (factorial 10)
`,

  solution: `factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = print (factorial 10)
`,

  tests: [
    {
      name: "factorial 10 = 3628800",
      expected: "3628800\n",
    },
    {
      name: "factorial 0 = 1",
      code: `{{FUNC}}
main :: IO ()
main = print (factorial 0)
`,
      expected: "1\n",
    },
    {
      name: "factorial 5 = 120",
      code: `{{FUNC}}
main :: IO ()
main = print (factorial 5)
`,
      expected: "120\n",
    },
  ],
};
