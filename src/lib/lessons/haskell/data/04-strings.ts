import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "basics",
  content: `## Strings in Haskell

In Haskell, \`String\` is an alias for \`[Char]\` — a list of characters. This gives you all list operations for free.

### Concatenation

Use \`++\` to concatenate strings:

\`\`\`haskell
putStrLn ("Hello" ++ ", " ++ "World!")
-- Hello, World!
\`\`\`

### Useful Functions

\`\`\`haskell
length "hello"        -- 5
reverse "hello"       -- "olleh"
words "foo bar baz"   -- ["foo","bar","baz"]
unwords ["foo","bar"] -- "foo bar"
\`\`\`

### Converting Numbers to Strings

Use \`show\` to convert a number to its string representation:

\`\`\`haskell
let msg = "The answer is " ++ show 42
putStrLn msg   -- The answer is 42
\`\`\`

### Your Task

Build the string \`"Haskell has 7 letters"\` using \`++\` and \`show\`, then print it. (Use \`length "Haskell"\` to get the count.)`,

  starterCode: `main :: IO ()
main = do
  let word = "Haskell"
  let msg = word ++ " has " ++ show (length word) ++ " letters"
  putStrLn msg
`,

  solution: `main :: IO ()
main = do
  let word = "Haskell"
  let msg = word ++ " has " ++ show (length word) ++ " letters"
  putStrLn msg
`,

  tests: [
    {
      name: "prints Haskell has 7 letters",
      expected: "Haskell has 7 letters\n",
    },
  ],
};
