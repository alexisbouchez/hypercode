import type { Lesson } from "../../types";

export const whileAndRepeat: Lesson = {
  id: "while-and-repeat",
  title: "While & Repeat",
  chapterId: "control-flow",
  content: `## While Loop

The \`while\` loop checks its condition before each iteration:

\`\`\`lua
local i = 1
while i <= 5 do
  print(i)
  i = i + 1
end
\`\`\`

## Repeat-Until

The \`repeat\` loop checks its condition **after** each iteration (like do-while in other languages):

\`\`\`lua
local i = 1
repeat
  print(i)
  i = i + 1
until i > 5
\`\`\`

The body always runs at least once.

### Your Task

Use a \`while\` loop to print the numbers 1 through 5, one per line.`,

  starterCode: `local i = 1
while i <= 5 do
  print(i)
  i = i + 1
end
`,

  solution: `local i = 1
while i <= 5 do
  print(i)
  i = i + 1
end
`,

  tests: [
    {
      name: "prints 1 to 5",
      expected: "1\n2\n3\n4\n5\n",
    },
  ],
};
