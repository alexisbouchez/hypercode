import type { Lesson } from "../../types";

export const lambdas: Lesson = {
  id: "lambdas",
  title: "Lambdas",
  chapterId: "functional",
  content: `## Lambda Expressions

Lambdas (anonymous functions) use backslash notation — the \`\\\` resembles the Greek letter λ:

\`\`\`haskell
\\x -> x + 1          -- adds 1
\\x y -> x * y        -- multiplies two numbers
\`\`\`

### With Higher-Order Functions

Lambdas are most useful when passed to \`map\`, \`filter\`, \`foldl\`, etc.:

\`\`\`haskell
map (\\x -> x * x) [1..5]
-- [1,4,9,16,25]

filter (\\x -> x > 3) [1..6]
-- [4,5,6]
\`\`\`

### Partial Application

Often, a lambda can be replaced with partial application:

\`\`\`haskell
map (\\x -> x + 10) [1,2,3]
-- same as:
map (+10) [1,2,3]
-- [11,12,13]
\`\`\`

### Your Task

Use \`map\` with a lambda to convert a list of temperatures in Celsius to Fahrenheit. Formula: \`f = c * 9 / 5 + 32\`. Print the result for \`[0, 20, 37, 100]\`.`,

  starterCode: `main :: IO ()
main = do
  let celsius = [0, 20, 37, 100]
  let fahrenheit = map (\\c -> c * 9 \`div\` 5 + 32) celsius
  print fahrenheit
`,

  solution: `main :: IO ()
main = do
  let celsius = [0, 20, 37, 100]
  let fahrenheit = map (\\c -> c * 9 \`div\` 5 + 32) celsius
  print fahrenheit
`,

  tests: [
    {
      name: "converts Celsius to Fahrenheit",
      expected: "[32,68,98,212]\n",
    },
  ],
};
