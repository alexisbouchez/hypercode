import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "basics",
  content: `## Your First Haskell Program

The entry point of every Haskell program is \`main\`, an IO action. To print a line of text, use \`putStrLn\`:

\`\`\`haskell
main :: IO ()
main = putStrLn "Hello, World!"
\`\`\`

\`putStrLn\` prints a string followed by a newline. You can also use:
- \`putStr\` — prints without a newline
- \`print\` — prints any showable value (adds quotes around strings)

\`\`\`haskell
main = do
  putStrLn "Hello"   -- Hello
  putStr "no newline"
  print 42           -- 42
\`\`\`

### Comments

Single-line comments start with \`--\`:

\`\`\`haskell
-- This is a comment
main = putStrLn "code"  -- inline comment
\`\`\`

### Your Task

Print exactly \`Hello, World!\` using \`putStrLn\`.`,

  starterCode: `main :: IO ()
main = putStrLn "Hello, World!"
`,

  solution: `main :: IO ()
main = putStrLn "Hello, World!"
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
