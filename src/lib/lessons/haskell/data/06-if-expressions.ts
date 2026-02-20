import type { Lesson } from "../../types";

export const ifExpressions: Lesson = {
  id: "if-expressions",
  title: "If Expressions",
  chapterId: "functions",
  content: `## If Expressions

In Haskell, \`if\` is an **expression**, not a statement. It always produces a value. Both branches must exist and have the same type:

\`\`\`haskell
absolute :: Int -> Int
absolute n = if n < 0 then -n else n
\`\`\`

Unlike many languages, the \`else\` branch is mandatory.

### In do-blocks

You can use \`if\` inside a \`do\` block too:

\`\`\`haskell
main :: IO ()
main = do
  let x = 10
  putStrLn (if x > 5 then "big" else "small")
\`\`\`

### Nesting

\`\`\`haskell
classify :: Int -> String
classify n = if n < 0 then "negative"
             else if n == 0 then "zero"
             else "positive"
\`\`\`

### Your Task

Define \`maxOf\` that returns the larger of two integers using \`if/then/else\`. Then print \`maxOf 17 42\`.`,

  starterCode: `maxOf :: Int -> Int -> Int
maxOf a b = if a > b then a else b

main :: IO ()
main = print (maxOf 17 42)
`,

  solution: `maxOf :: Int -> Int -> Int
maxOf a b = if a > b then a else b

main :: IO ()
main = print (maxOf 17 42)
`,

  tests: [
    {
      name: "maxOf 17 42 = 42",
      expected: "42\n",
    },
    {
      name: "maxOf picks larger",
      code: `{{FUNC}}
main :: IO ()
main = print (maxOf 99 1)
`,
      expected: "99\n",
    },
  ],
};
