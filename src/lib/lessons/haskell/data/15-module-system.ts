import type { Lesson } from "../../types";

export const moduleSystem: Lesson = {
  id: "module-system",
  title: "Module System",
  chapterId: "functional",
  content: `## Module System

Haskell's standard library is organized into **modules**. By default, only \`Prelude\` is imported. To use functions from other modules, you need an \`import\` statement at the top of your file.

### Basic Imports

Import an entire module to bring all its exports into scope:

\`\`\`haskell
import Data.Char
import Data.List
\`\`\`

Now you can use \`toUpper\`, \`sort\`, \`nub\`, etc. directly.

### Importing Specific Functions

To avoid cluttering your namespace, import only what you need:

\`\`\`haskell
import Data.Char (toUpper, isAlpha)
import Data.List (sort, nub)
\`\`\`

### Qualified Imports

When two modules export functions with the same name, use qualified imports:

\`\`\`haskell
import qualified Data.Map as Map

myMap = Map.fromList [("a", 1), ("b", 2)]
value = Map.lookup "a" myMap  -- Just 1
\`\`\`

With \`qualified\`, you must always use the prefix: \`Map.lookup\`, not just \`lookup\`.

### Common Modules

| Module | Key Functions |
|--------|--------------|
| \`Data.Char\` | \`toUpper\`, \`toLower\`, \`isAlpha\`, \`isDigit\` |
| \`Data.List\` | \`sort\`, \`nub\`, \`group\`, \`intercalate\` |
| \`Data.Map\` | \`fromList\`, \`lookup\`, \`insert\`, \`member\` |
| \`Data.Maybe\` | \`fromMaybe\`, \`isJust\`, \`isNothing\` |

### Your Task

1. Import \`Data.Char\` (specifically \`toUpper\` and \`isAlpha\`)
2. Import \`Data.List\` (specifically \`sort\` and \`intercalate\`)
3. Define \`shout\` that converts a string to uppercase using \`map toUpper\`
4. Define \`onlyLetters\` that filters a string to only alphabetic characters using \`filter isAlpha\`
5. Print \`shout "hello"\`
6. Print \`onlyLetters "h3ll0 w0rld"\`
7. Print \`sort [3, 1, 4, 1, 5]\`
8. Print \`intercalate ", " ["one", "two", "three"]\``,

  starterCode: `import Data.Char (toUpper, isAlpha)
import Data.List (sort, intercalate)

shout :: String -> String
shout = map toUpper

onlyLetters :: String -> String
onlyLetters = filter isAlpha

main :: IO ()
main = do
  putStrLn (shout "hello")
  putStrLn (onlyLetters "h3ll0 w0rld")
  print (sort [3, 1, 4, 1, 5])
  putStrLn (intercalate ", " ["one", "two", "three"])
`,

  solution: `import Data.Char (toUpper, isAlpha)
import Data.List (sort, intercalate)

shout :: String -> String
shout = map toUpper

onlyLetters :: String -> String
onlyLetters = filter isAlpha

main :: IO ()
main = do
  putStrLn (shout "hello")
  putStrLn (onlyLetters "h3ll0 w0rld")
  print (sort [3, 1, 4, 1, 5])
  putStrLn (intercalate ", " ["one", "two", "three"])
`,

  tests: [
    {
      name: "shout and module functions",
      expected: "HELLO\nhllwrld\n[1,1,3,4,5]\none, two, three\n",
    },
    {
      name: "shout on mixed case",
      code: `import Data.Char (toUpper)
{{FUNC}}
main :: IO ()
main = putStrLn (shout "Hello World")
`,
      expected: "HELLO WORLD\n",
    },
    {
      name: "onlyLetters filters digits and spaces",
      code: `import Data.Char (isAlpha)
{{FUNC}}
main :: IO ()
main = putStrLn (onlyLetters "a1b2c3")
`,
      expected: "abc\n",
    },
  ],
};
