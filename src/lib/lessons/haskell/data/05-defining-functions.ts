import type { Lesson } from "../../types";

export const definingFunctions: Lesson = {
  id: "defining-functions",
  title: "Defining Functions",
  chapterId: "functions",
  content: `## Defining Functions

Functions in Haskell are defined at the top level, outside of \`main\`:

\`\`\`haskell
add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = print (add 3 4)   -- 7
\`\`\`

The type signature \`Int -> Int -> Int\` means: takes two \`Int\`s, returns an \`Int\`. Type signatures are optional (Haskell infers them) but good practice.

### Calling Functions

Function application uses a space — no parentheses needed unless grouping:

\`\`\`haskell
add 3 4        -- 7
add (add 1 2) 4  -- 7  (grouped)
\`\`\`

### Multiple Arguments

Haskell functions are curried: \`add 3\` returns a function that adds 3 to its argument:

\`\`\`haskell
addThree = add 3
addThree 10    -- 13
\`\`\`

### Your Task

Define a function \`double\` that multiplies its argument by 2, then print \`double 21\`.`,

  starterCode: `double :: Int -> Int
double x = x * 2

main :: IO ()
main = print (double 21)
`,

  solution: `double :: Int -> Int
double x = x * 2

main :: IO ()
main = print (double 21)
`,

  tests: [
    {
      name: "double 21 = 42",
      expected: "42\n",
    },
    {
      name: "double works",
      code: `{{FUNC}}
main :: IO ()
main = print (double 5)
`,
      expected: "10\n",
    },
  ],
};
