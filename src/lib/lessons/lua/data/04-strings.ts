import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "basics",
  content: `## Strings

Strings in Lua are enclosed in double or single quotes. Concatenate with \`..\`:

\`\`\`lua
local greeting = "Hello"
local name = "World"
print(greeting .. ", " .. name .. "!")
\`\`\`

### String Library

Lua provides a \`string\` library with useful functions:

\`\`\`lua
print(string.len("hello"))       -- 5
print(string.upper("hello"))     -- HELLO
print(string.lower("HELLO"))     -- hello
print(string.rep("ha", 3))       -- hahaha
print(string.sub("hello", 2, 4)) -- ell
print(string.reverse("hello"))   -- olleh
\`\`\`

### String Format

\`string.format\` works like C's \`printf\`:

\`\`\`lua
print(string.format("Name: %s, Age: %d", "Alice", 30))
\`\`\`

### Your Task

Use string operations to produce the expected output.`,

  starterCode: `local word = "Lua"

print(string.upper(word))
print(string.rep(word, 3))
print(string.len(word))
`,

  solution: `local word = "Lua"

print(string.upper(word))
print(string.rep(word, 3))
print(string.len(word))
`,

  tests: [
    {
      name: "string operations",
      expected: "LUA\nLuaLuaLua\n3\n",
    },
  ],
};
