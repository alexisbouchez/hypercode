import type { Lesson } from "../../types";

export const closures: Lesson = {
  id: "closures",
  title: "Closures",
  chapterId: "functions",
  content: `## Closures

A closure is a function that captures variables from its enclosing scope. In Lua, functions are first-class values and naturally form closures:

\`\`\`lua
function makeCounter()
  local count = 0
  return function()
    count = count + 1
    return count
  end
end

local counter = makeCounter()
print(counter())  -- 1
print(counter())  -- 2
print(counter())  -- 3
\`\`\`

Each call to \`makeCounter()\` creates a new, independent counter.

### Anonymous Functions

Functions can be stored in variables:

\`\`\`lua
local double = function(x)
  return x * 2
end
print(double(5))  -- 10
\`\`\`

### Your Task

Write a function \`makeAdder\` that takes a number \`n\` and returns a function that adds \`n\` to its argument.`,

  starterCode: `function makeAdder(n)
  return function(x)
    return n + x
  end
end

local add5 = makeAdder(5)
local add10 = makeAdder(10)

print(add5(3))
print(add10(7))
print(add5(0))
`,

  solution: `function makeAdder(n)
  return function(x)
    return n + x
  end
end

local add5 = makeAdder(5)
local add10 = makeAdder(10)

print(add5(3))
print(add10(7))
print(add5(0))
`,

  tests: [
    {
      name: "add5(3)",
      expected: "8\n",
      code: `{{FUNC}}
local add5 = makeAdder(5)
print(add5(3))
`,
    },
    {
      name: "add10(7)",
      expected: "17\n",
      code: `{{FUNC}}
local add10 = makeAdder(10)
print(add10(7))
`,
    },
    {
      name: "add5(0)",
      expected: "5\n",
      code: `{{FUNC}}
local add5 = makeAdder(5)
print(add5(0))
`,
    },
  ],
};
