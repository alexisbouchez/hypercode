import type { Lesson } from "../../types";

export const variablesAndTypes: Lesson = {
  id: "variables-and-types",
  title: "Variables & Types",
  chapterId: "basics",
  content: `## Variables & Types

Lua is dynamically typed. Variables are declared with \`local\`:

\`\`\`lua
local name = "Alice"
local age = 30
local active = true
local nothing = nil
\`\`\`

Lua has these basic types: \`nil\`, \`boolean\`, \`number\`, \`string\`, \`table\`, and \`function\`.

Use \`type()\` to check a value's type:

\`\`\`lua
print(type(42))       -- number
print(type("hello"))  -- string
print(type(true))     -- boolean
print(type(nil))      -- nil
\`\`\`

### Your Task

Create variables for a name (string), an age (number), and print them on separate lines.`,

  starterCode: `local name = "Bob"
local age = 25

print(name)
print(age)
print(type(name))
`,

  solution: `local name = "Bob"
local age = 25

print(name)
print(age)
print(type(name))
`,

  tests: [
    {
      name: "prints name",
      expected: "Bob\n25\nstring\n",
    },
  ],
};
