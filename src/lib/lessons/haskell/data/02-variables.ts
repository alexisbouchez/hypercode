import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables",
  chapterId: "basics",
  content: `## Variables in Haskell

In Haskell, variables are immutable bindings. Once bound, a value never changes. You bind values inside a \`do\` block using \`let\`:

\`\`\`haskell
main :: IO ()
main = do
  let name = "Alice"
  let age = 30
  putStrLn name
  print age
\`\`\`

### Types

Haskell is statically typed. Common types:

| Value | Type |
|-------|------|
| \`42\` | \`Int\` |
| \`3.14\` | \`Double\` |
| \`"hello"\` | \`String\` |
| \`True\` | \`Bool\` |

You can optionally annotate types:

\`\`\`haskell
let x :: Int
    x = 42
\`\`\`

### show

\`show\` converts any showable value to a \`String\`:

\`\`\`haskell
putStrLn (show 42)   -- "42"
\`\`\`

### Your Task

Bind \`name\` to \`"Haskell"\` and \`year\` to \`1990\`, then print them each on their own line using \`putStrLn\` and \`show\`.`,

  starterCode: `main :: IO ()
main = do
  let name = "Haskell"
  let year = 1990
  putStrLn name
  putStrLn (show year)
`,

  solution: `main :: IO ()
main = do
  let name = "Haskell"
  let year = 1990
  putStrLn name
  putStrLn (show year)
`,

  tests: [
    {
      name: "prints name and year",
      expected: "Haskell\n1990\n",
    },
  ],
};
