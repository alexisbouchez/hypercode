import type { Lesson } from "../../types";

export const whereLet: Lesson = {
  id: "where-let",
  title: "Where and Let",
  chapterId: "functional",
  content: `## Where and Let

Haskell has two ways to introduce local bindings: \`where\` and \`let ... in\`.

### where

\`where\` goes at the end of a function definition and scopes over the whole body:

\`\`\`haskell
bmiCategory :: Double -> String
bmiCategory weight
  | bmi < 18.5  = "Underweight"
  | bmi < 25.0  = "Normal"
  | otherwise   = "Overweight"
  where
    bmi = weight / (1.75 * 1.75)
\`\`\`

### let ... in

\`let ... in\` is an expression — it can appear anywhere an expression is expected:

\`\`\`haskell
circleArea :: Double -> Double
circleArea r =
  let pi_ = 3.14159
      r2  = r * r
  in pi_ * r2
\`\`\`

### In do-blocks

Inside \`do\`, use just \`let\` (without \`in\`):

\`\`\`haskell
main = do
  let x = 10
      y = 20
  print (x + y)
\`\`\`

### Your Task

Define \`hypotenuse\` that computes the hypotenuse \`sqrt(a² + b²)\` using a \`where\` clause for intermediate values. Print \`hypotenuse 3 4\`.`,

  starterCode: `hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt sumOfSquares
  where
    sumOfSquares = a * a + b * b

main :: IO ()
main = print (hypotenuse 3 4)
`,

  solution: `hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt sumOfSquares
  where
    sumOfSquares = a * a + b * b

main :: IO ()
main = print (hypotenuse 3 4)
`,

  tests: [
    {
      name: "hypotenuse 3 4 = 5.0",
      expected: "5.0\n",
    },
  ],
};
