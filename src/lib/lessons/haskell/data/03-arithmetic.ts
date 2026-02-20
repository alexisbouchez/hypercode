import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
  id: "arithmetic",
  title: "Arithmetic",
  chapterId: "basics",
  content: `## Arithmetic in Haskell

Haskell supports the standard arithmetic operators:

\`\`\`haskell
main :: IO ()
main = do
  print (2 + 3)    -- 5
  print (10 - 4)   -- 6
  print (3 * 7)    -- 21
  print (15 \`div\` 4)  -- 3  (integer division)
  print (15 \`mod\` 4)  -- 3  (remainder)
  print (2 ^ 10)   -- 1024
\`\`\`

### Integer vs Float Division

Use \`div\` for integer division (like \`//\` in Python). The \`/\` operator is for floating-point numbers:

\`\`\`haskell
print (7 \`div\` 2)   -- 3
print (7.0 / 2.0)  -- 3.5
\`\`\`

### Negative Numbers

Wrap negatives in parentheses when passing to functions:

\`\`\`haskell
print (abs (-5))   -- 5
print (negate 3)   -- -3
\`\`\`

### Your Task

Compute and print:
1. \`100 \`div\` 7\`
2. \`100 \`mod\` 7\`
3. \`2 ^ 8\``,

  starterCode: `main :: IO ()
main = do
  print (100 \`div\` 7)
  print (100 \`mod\` 7)
  print (2 ^ 8)
`,

  solution: `main :: IO ()
main = do
  print (100 \`div\` 7)
  print (100 \`mod\` 7)
  print (2 ^ 8)
`,

  tests: [
    {
      name: "prints 14, 2, 256",
      expected: "14\n2\n256\n",
    },
  ],
};
