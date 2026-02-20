import type { Lesson } from "../../types";

export const folds: Lesson = {
  id: "folds",
  title: "Folds",
  chapterId: "lists",
  content: `## Folds

Folds reduce a list to a single value by repeatedly applying a function.

### foldl

\`foldl\` processes left-to-right with an accumulator:

\`\`\`haskell
foldl (+) 0 [1,2,3,4,5]   -- 15
foldl (*) 1 [1..5]         -- 120
\`\`\`

The signature: \`foldl :: (b -> a -> b) -> b -> [a] -> b\`
- First arg: combining function \`(accumulator -> element -> new_accumulator)\`
- Second arg: initial accumulator
- Third arg: the list

### foldr

\`foldr\` processes right-to-left:

\`\`\`haskell
foldr (:) [] [1,2,3]   -- [1,2,3]  (identity on lists)
\`\`\`

### Handy Shortcuts

\`sum\` and \`product\` are built on folds:

\`\`\`haskell
sum [1..100]      -- 5050
product [1..5]    -- 120
\`\`\`

### Your Task

Use \`foldl\` to compute the sum of squares of \`[1..5]\` (1 + 4 + 9 + 16 + 25 = 55). Print the result.`,

  starterCode: `main :: IO ()
main = do
  let sumOfSquares = foldl (\\acc x -> acc + x * x) 0 [1..5]
  print sumOfSquares
`,

  solution: `main :: IO ()
main = do
  let sumOfSquares = foldl (\\acc x -> acc + x * x) 0 [1..5]
  print sumOfSquares
`,

  tests: [
    {
      name: "sum of squares 1..5 = 55",
      expected: "55\n",
    },
  ],
};
