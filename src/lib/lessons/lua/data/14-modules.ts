import type { Lesson } from "../../types";

export const modules: Lesson = {
  id: "modules",
  title: "Modules",
  chapterId: "advanced",
  content: `## Modules

In Lua, a module is just a table containing functions and values. The conventional pattern is:

\`\`\`lua
local M = {}

function M.greet(name)
  return "Hello, " .. name
end

function M.farewell(name)
  return "Goodbye, " .. name
end

return M
\`\`\`

You access module functions with dot notation: \`M.greet("Lua")\`.

### Encapsulation

Local functions inside the module file are private:

\`\`\`lua
local M = {}

local function helper(x)
  return x * 2
end

function M.compute(x)
  return helper(x) + 1
end
\`\`\`

\`helper\` is not accessible outside the module, but \`M.compute\` is.

### Your Task

Create a simple math module with \`add\` and \`sub\` functions.`,

  starterCode: `local MyMath = {}

function MyMath.add(a, b)
  return a + b
end

function MyMath.sub(a, b)
  return a - b
end

print(MyMath.add(10, 5))
print(MyMath.sub(10, 5))
print(MyMath.add(3, MyMath.sub(10, 7)))
`,

  solution: `local MyMath = {}

function MyMath.add(a, b)
  return a + b
end

function MyMath.sub(a, b)
  return a - b
end

print(MyMath.add(10, 5))
print(MyMath.sub(10, 5))
print(MyMath.add(3, MyMath.sub(10, 7)))
`,

  tests: [
    {
      name: "add(10, 5)",
      expected: "15\n",
      code: `{{FUNC}}
print(MyMath.add(10, 5))
`,
    },
    {
      name: "sub(10, 5)",
      expected: "5\n",
      code: `{{FUNC}}
print(MyMath.sub(10, 5))
`,
    },
    {
      name: "add(3, sub(10, 7))",
      expected: "6\n",
      code: `{{FUNC}}
print(MyMath.add(3, MyMath.sub(10, 7)))
`,
    },
  ],
};
