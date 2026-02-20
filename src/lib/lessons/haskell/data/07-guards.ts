import type { Lesson } from "../../types";

export const guards: Lesson = {
  id: "guards",
  title: "Guards",
  chapterId: "functions",
  content: `## Guards

Guards are a clean alternative to nested \`if/then/else\`. They use \`|\` to list conditions:

\`\`\`haskell
bmi :: Double -> String
bmi b
  | b < 18.5  = "Underweight"
  | b < 25.0  = "Normal"
  | b < 30.0  = "Overweight"
  | otherwise = "Obese"
\`\`\`

\`otherwise\` is just \`True\` — a catch-all guard.

Guards are evaluated top-to-bottom; the first matching branch wins.

### Compared to if/then/else

Guards are preferred when you have three or more conditions:

\`\`\`haskell
-- Harder to read:
sign n = if n > 0 then "positive" else if n < 0 then "negative" else "zero"

-- Cleaner with guards:
sign :: Int -> String
sign n
  | n > 0     = "positive"
  | n < 0     = "negative"
  | otherwise = "zero"
\`\`\`

### Your Task

Define \`grade\` that maps a score to a letter grade:
- 90–100 → \`"A"\`
- 80–89 → \`"B"\`
- 70–79 → \`"C"\`
- otherwise → \`"F"\`

Then print \`grade 85\`.`,

  starterCode: `grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | otherwise   = "F"

main :: IO ()
main = putStrLn (grade 85)
`,

  solution: `grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | otherwise   = "F"

main :: IO ()
main = putStrLn (grade 85)
`,

  tests: [
    {
      name: "grade 85 = B",
      expected: "B\n",
    },
    {
      name: "grade 95 = A",
      code: `{{FUNC}}
main :: IO ()
main = putStrLn (grade 95)
`,
      expected: "A\n",
    },
    {
      name: "grade 55 = F",
      code: `{{FUNC}}
main :: IO ()
main = putStrLn (grade 55)
`,
      expected: "F\n",
    },
  ],
};
