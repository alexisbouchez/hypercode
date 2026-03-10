import type { Lesson } from "../../types";

export const helloLua: Lesson = {
  id: "hello-lua",
  title: "Hello, Lua!",
  chapterId: "basics",
  content: `## Your First Lua Program

In Lua, \`print\` outputs values followed by a newline:

\`\`\`lua
print("Hello, World!")
\`\`\`

You can print numbers, booleans, and combine strings with the \`..\` concatenation operator:

\`\`\`lua
print("Hello, " .. "Lua!")
print(42)
print(true)
\`\`\`

### Comments

\`\`\`lua
-- This is a single-line comment
\`\`\`

### Your Task

Print exactly \`Hello, Lua!\`.`,

  starterCode: `print("Hello, Lua!")
`,

  solution: `print("Hello, Lua!")
`,

  tests: [
    {
      name: "prints Hello, Lua!",
      expected: "Hello, Lua!\n",
    },
  ],
};
