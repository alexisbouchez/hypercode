import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions

Functions in Lua are defined with the \`function\` keyword:

\`\`\`lua
function greet(name)
  print("Hello, " .. name .. "!")
end

greet("Lua")  -- Hello, Lua!
\`\`\`

Functions can return values:

\`\`\`lua
function add(a, b)
  return a + b
end

print(add(3, 4))  -- 7
\`\`\`

### Local Functions

Use \`local\` to restrict a function's scope:

\`\`\`lua
local function square(x)
  return x * x
end
\`\`\`

### Your Task

Write a function \`multiply\` that takes two numbers and returns their product.`,

  starterCode: `function multiply(a, b)
  return a * b
end

print(multiply(3, 4))
print(multiply(5, 6))
`,

  solution: `function multiply(a, b)
  return a * b
end

print(multiply(3, 4))
print(multiply(5, 6))
`,

  tests: [
    {
      name: "multiply 3 * 4",
      expected: "12\n",
      code: `{{FUNC}}
print(multiply(3, 4))
`,
    },
    {
      name: "multiply 5 * 6",
      expected: "30\n",
      code: `{{FUNC}}
print(multiply(5, 6))
`,
    },
    {
      name: "multiply 0 * 99",
      expected: "0\n",
      code: `{{FUNC}}
print(multiply(0, 99))
`,
    },
  ],
};
