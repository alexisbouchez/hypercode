import type { Lesson } from "../../types";

export const ifElseifElse: Lesson = {
  id: "if-elseif-else",
  title: "If / Elseif / Else",
  chapterId: "control-flow",
  content: `## Conditional Statements

Lua uses \`if\`, \`elseif\`, \`else\`, and \`end\`:

\`\`\`lua
local x = 10

if x > 0 then
  print("positive")
elseif x < 0 then
  print("negative")
else
  print("zero")
end
\`\`\`

### Comparison Operators

- \`==\` (equal), \`~=\` (not equal)
- \`<\`, \`>\`, \`<=\`, \`>=\`

### Logical Operators

- \`and\`, \`or\`, \`not\`

\`\`\`lua
if x > 0 and x < 100 then
  print("between 1 and 99")
end
\`\`\`

### Your Task

Write a function that classifies a number as "positive", "negative", or "zero".`,

  starterCode: `function classify(n)
  if n > 0 then
    print("positive")
  elseif n < 0 then
    print("negative")
  else
    print("zero")
  end
end

classify(5)
classify(-3)
classify(0)
`,

  solution: `function classify(n)
  if n > 0 then
    print("positive")
  elseif n < 0 then
    print("negative")
  else
    print("zero")
  end
end

classify(5)
classify(-3)
classify(0)
`,

  tests: [
    {
      name: "positive",
      expected: "positive\n",
      code: `{{FUNC}}
classify(5)
`,
    },
    {
      name: "negative",
      expected: "negative\n",
      code: `{{FUNC}}
classify(-3)
`,
    },
    {
      name: "zero",
      expected: "zero\n",
      code: `{{FUNC}}
classify(0)
`,
    },
  ],
};
