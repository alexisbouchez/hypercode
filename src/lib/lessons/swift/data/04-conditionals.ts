import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "basics",
  content: `## Conditionals

Swift \`if\`/\`else if\`/\`else\` work like most languages:

\`\`\`swift
let score = 85

if score >= 90 {
    print("A")
} else if score >= 80 {
    print("B")
} else {
    print("C")
}
// B
\`\`\`

### Comparison Operators

\`==\`, \`!=\`, \`<\`, \`>\`, \`<=\`, \`>=\`

### Logical Operators

\`&&\` (and), \`||\` (or), \`!\` (not)

### Your Task

Write a function \`sign\` that takes an \`Int\` and returns:
- \`"positive"\` if the number is greater than 0
- \`"negative"\` if less than 0
- \`"zero"\` if equal to 0`,

  starterCode: `func sign(_ n: Int) -> String {
    if n > 0 {
        return "positive"
    } else if n < 0 {
        return "negative"
    } else {
        return "zero"
    }
}

print(sign(5))
print(sign(-3))
print(sign(0))
`,

  solution: `func sign(_ n: Int) -> String {
    if n > 0 {
        return "positive"
    } else if n < 0 {
        return "negative"
    } else {
        return "zero"
    }
}

print(sign(5))
print(sign(-3))
print(sign(0))
`,

  tests: [
    {
      name: "positive number",
      expected: "positive\n",
      code: `{{FUNC}}
print(sign(5))
`,
    },
    {
      name: "negative number",
      expected: "negative\n",
      code: `{{FUNC}}
print(sign(-3))
`,
    },
    {
      name: "zero",
      expected: "zero\n",
      code: `{{FUNC}}
print(sign(0))
`,
    },
  ],
};
