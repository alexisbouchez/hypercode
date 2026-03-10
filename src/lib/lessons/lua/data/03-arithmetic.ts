import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
  id: "arithmetic",
  title: "Arithmetic",
  chapterId: "basics",
  content: `## Arithmetic

Lua supports the usual arithmetic operators:

\`\`\`lua
print(10 + 3)   -- 13
print(10 - 3)   -- 7
print(10 * 3)   -- 30
print(10 / 3)   -- 3.3333333333333
print(10 % 3)   -- 1
print(2 ^ 10)   -- 1024
\`\`\`

The \`math\` library provides common functions:

\`\`\`lua
print(math.floor(3.7))  -- 3
print(math.ceil(3.2))   -- 4
print(math.abs(-5))     -- 5
print(math.sqrt(16))    -- 4
print(math.max(1, 5))   -- 5
print(math.min(1, 5))   -- 1
\`\`\`

### Your Task

Compute and print the results of the given expressions.`,

  starterCode: `local a = 15
local b = 4

print(a + b)
print(a * b)
print(math.floor(a / b))
`,

  solution: `local a = 15
local b = 4

print(a + b)
print(a * b)
print(math.floor(a / b))
`,

  tests: [
    {
      name: "addition",
      expected: "19\n60\n3\n",
    },
  ],
};
