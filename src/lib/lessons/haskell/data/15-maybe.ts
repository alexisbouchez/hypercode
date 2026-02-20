import type { Lesson } from "../../types";

export const maybe_: Lesson = {
  id: "maybe",
  title: "Maybe",
  chapterId: "functional",
  content: `## Maybe

\`Maybe\` represents a value that might be absent. Instead of \`null\`, Haskell uses:
- \`Just x\` — the value \`x\` is present
- \`Nothing\` — no value

\`\`\`haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a \`div\` b)
\`\`\`

### Pattern Matching on Maybe

\`\`\`haskell
describe :: Maybe Int -> String
describe Nothing  = "no value"
describe (Just x) = "got " ++ show x
\`\`\`

### fromMaybe

\`fromMaybe\` (from \`Data.Maybe\`) provides a default for \`Nothing\`:

\`\`\`haskell
import Data.Maybe (fromMaybe)

fromMaybe 0 (Just 42)   -- 42
fromMaybe 0 Nothing     -- 0
\`\`\`

### Your Task

Define \`safeHead\` that returns \`Just\` the first element of a list, or \`Nothing\` for an empty list. Then:
- Print \`fromMaybe (-1) (safeHead [10, 20, 30])\`
- Print \`fromMaybe (-1) (safeHead [])\``,

  starterCode: `import Data.Maybe (fromMaybe)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  print (fromMaybe (-1) (safeHead [10, 20, 30]))
  print (fromMaybe (-1) (safeHead []))
`,

  solution: `import Data.Maybe (fromMaybe)

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  print (fromMaybe (-1) (safeHead [10, 20, 30]))
  print (fromMaybe (-1) (safeHead []))
`,

  tests: [
    {
      name: "safeHead on non-empty and empty",
      expected: "10\n-1\n",
    },
    {
      name: "safeHead empty = Nothing",
      code: `{{FUNC}}
main :: IO ()
main = print (safeHead ([] :: [Int]))
`,
      expected: "Nothing\n",
    },
    {
      name: "safeHead non-empty = Just",
      code: `{{FUNC}}
main :: IO ()
main = print (safeHead [42])
`,
      expected: "Just 42\n",
    },
  ],
};
