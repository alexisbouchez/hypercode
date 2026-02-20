import type { Lesson } from "../../types";

export const listComprehensions: Lesson = {
  id: "list-comprehensions",
  title: "List Comprehensions",
  chapterId: "lists",
  content: `## List Comprehensions

List comprehensions offer a concise syntax for building lists, inspired by set-builder notation:

\`\`\`haskell
[x * 2 | x <- [1..5]]
-- [2,4,6,8,10]
\`\`\`

Read as: "for each \`x\` drawn from \`[1..5]\`, produce \`x * 2\`".

### With Guards

Add predicates after the generator:

\`\`\`haskell
[x | x <- [1..20], even x]
-- [2,4,6,8,10,12,14,16,18,20]

[x | x <- [1..30], x \`mod\` 3 == 0]
-- [3,6,9,12,15,18,21,24,27,30]
\`\`\`

### Multiple Generators

\`\`\`haskell
[(x, y) | x <- [1..3], y <- [1..3], x /= y]
-- [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
\`\`\`

### Your Task

Use a list comprehension to generate all perfect squares up to 100, then print them. (Squares: 1, 4, 9, ..., 100)`,

  starterCode: `main :: IO ()
main = do
  let squares = [x * x | x <- [1..10]]
  print squares
`,

  solution: `main :: IO ()
main = do
  let squares = [x * x | x <- [1..10]]
  print squares
`,

  tests: [
    {
      name: "perfect squares up to 100",
      expected: "[1,4,9,16,25,36,49,64,81,100]\n",
    },
  ],
};
