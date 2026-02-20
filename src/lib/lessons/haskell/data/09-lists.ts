import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "lists",
  content: `## Lists

Lists are Haskell's fundamental data structure. All elements must have the same type.

\`\`\`haskell
let nums = [1, 2, 3, 4, 5]
let greets = ["hello", "world"]
\`\`\`

### Ranges

Use \`[a..b]\` to generate a range:

\`\`\`haskell
[1..5]    -- [1,2,3,4,5]
[2,4..10] -- [2,4,6,8,10]
\`\`\`

### Basic Functions

\`\`\`haskell
head [1,2,3]   -- 1
tail [1,2,3]   -- [2,3]
length [1,2,3] -- 3
null []        -- True
null [1]       -- False
\`\`\`

### Cons Operator (:)

\`:\` prepends an element to a list:

\`\`\`haskell
1 : [2, 3]    -- [1,2,3]
0 : [1..4]    -- [0,1,2,3,4]
\`\`\`

### Your Task

Create the list \`[1..10]\` and print:
1. Its length
2. Its head
3. Its last element using \`last\``,

  starterCode: `main :: IO ()
main = do
  let xs = [1..10]
  print (length xs)
  print (head xs)
  print (last xs)
`,

  solution: `main :: IO ()
main = do
  let xs = [1..10]
  print (length xs)
  print (head xs)
  print (last xs)
`,

  tests: [
    {
      name: "prints 10, 1, 10",
      expected: "10\n1\n10\n",
    },
  ],
};
