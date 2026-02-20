import type { Lesson } from "../../types";

export const listOperations: Lesson = {
  id: "list-operations",
  title: "List Operations",
  chapterId: "lists",
  content: `## List Operations

Haskell's standard library has many useful list functions:

### map and filter

\`map\` applies a function to every element. \`filter\` keeps elements that satisfy a predicate:

\`\`\`haskell
map (*2) [1,2,3]        -- [2,4,6]
filter even [1..10]     -- [2,4,6,8,10]
\`\`\`

### take and drop

\`\`\`haskell
take 3 [1..10]   -- [1,2,3]
drop 3 [1..10]   -- [4,5,6,7,8,9,10]
\`\`\`

### zip

Pairs up elements from two lists:

\`\`\`haskell
zip [1,2,3] ["a","b","c"]
-- [(1,"a"),(2,"b"),(3,"c")]
\`\`\`

### reverse

\`\`\`haskell
reverse [1..5]   -- [5,4,3,2,1]
\`\`\`

### Your Task

Starting from \`[1..20]\`:
1. Filter to keep only odd numbers
2. Take the first 5 of those
3. Print the result`,

  starterCode: `main :: IO ()
main = do
  let xs = [1..20]
  let odds = filter odd xs
  let result = take 5 odds
  print result
`,

  solution: `main :: IO ()
main = do
  let xs = [1..20]
  let odds = filter odd xs
  let result = take 5 odds
  print result
`,

  tests: [
    {
      name: "first 5 odd numbers",
      expected: "[1,3,5,7,9]\n",
    },
  ],
};
