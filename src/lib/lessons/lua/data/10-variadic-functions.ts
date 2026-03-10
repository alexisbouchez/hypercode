import type { Lesson } from "../../types";

export const variadicFunctions: Lesson = {
  id: "variadic-functions",
  title: "Variadic Functions",
  chapterId: "functions",
  content: `## Variadic Functions

Lua functions can accept a variable number of arguments using \`...\`:

\`\`\`lua
function sum(...)
  local total = 0
  local n = select("#", ...)
  -- select("#", ...) returns the number of arguments
  return total
end
\`\`\`

The \`...\` collects all arguments. A simple way to work with variadic arguments is to pass them directly to other functions:

\`\`\`lua
function printAll(...)
  print(...)   -- passes all args to print
end
\`\`\`

### Combining Fixed and Variadic Parameters

\`\`\`lua
function greetAll(greeting, name1, name2)
  print(greeting .. ", " .. name1 .. "!")
  print(greeting .. ", " .. name2 .. "!")
end
\`\`\`

### Your Task

Write three functions: \`add3\` that adds three numbers, \`mul3\` that multiplies three, and \`sub\` that subtracts.`,

  starterCode: `function add3(a, b, c)
  return a + b + c
end

function mul3(a, b, c)
  return a * b * c
end

function sub(a, b)
  return a - b
end

print(add3(1, 2, 3))
print(mul3(2, 3, 4))
print(sub(10, 3))
`,

  solution: `function add3(a, b, c)
  return a + b + c
end

function mul3(a, b, c)
  return a * b * c
end

function sub(a, b)
  return a - b
end

print(add3(1, 2, 3))
print(mul3(2, 3, 4))
print(sub(10, 3))
`,

  tests: [
    {
      name: "add3(1, 2, 3)",
      expected: "6\n",
      code: `{{FUNC}}
print(add3(1, 2, 3))
`,
    },
    {
      name: "mul3(2, 3, 4)",
      expected: "24\n",
      code: `{{FUNC}}
print(mul3(2, 3, 4))
`,
    },
    {
      name: "sub(10, 3)",
      expected: "7\n",
      code: `{{FUNC}}
print(sub(10, 3))
`,
    },
  ],
};
