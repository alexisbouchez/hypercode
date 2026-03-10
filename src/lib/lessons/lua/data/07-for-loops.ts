import type { Lesson } from "../../types";

export const forLoops: Lesson = {
  id: "for-loops",
  title: "For Loops",
  chapterId: "control-flow",
  content: `## Numeric For

The numeric \`for\` loop iterates from a start to a stop value:

\`\`\`lua
for i = 1, 5 do
  print(i)
end
-- prints 1, 2, 3, 4, 5
\`\`\`

You can specify a step:

\`\`\`lua
for i = 0, 10, 2 do
  print(i)
end
-- prints 0, 2, 4, 6, 8, 10
\`\`\`

Counting down with a negative step:

\`\`\`lua
for i = 5, 1, -1 do
  print(i)
end
-- prints 5, 4, 3, 2, 1
\`\`\`

### Your Task

Use a for loop to print the even numbers from 2 to 10.`,

  starterCode: `for i = 2, 10, 2 do
  print(i)
end
`,

  solution: `for i = 2, 10, 2 do
  print(i)
end
`,

  tests: [
    {
      name: "even numbers 2 to 10",
      expected: "2\n4\n6\n8\n10\n",
    },
  ],
};
