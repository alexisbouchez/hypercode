import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
  id: "arithmetic",
  title: "Arithmetic",
  chapterId: "foundations",
  content: `## Arithmetic Operators

R supports all standard arithmetic operators:

| Operator | Description | Example |
|----------|-------------|---------|
| \`+\` | Addition | \`5 + 3\` is \`8\` |
| \`-\` | Subtraction | \`5 - 3\` is \`2\` |
| \`*\` | Multiplication | \`5 * 3\` is \`15\` |
| \`/\` | Division | \`7 / 2\` is \`3.5\` |
| \`%%\` | Modulo (remainder) | \`7 %% 2\` is \`1\` |
| \`%/%\` | Integer division | \`7 %/% 2\` is \`3\` |
| \`^\` | Exponentiation | \`2 ^ 3\` is \`8\` |

### Math Functions

R has many built-in math functions:

\`\`\`r
cat(abs(-5), "\\n")    # 5
cat(sqrt(16), "\\n")   # 4
cat(ceiling(3.2), "\\n") # 4
cat(floor(3.8), "\\n")   # 3
cat(round(3.567, 2), "\\n") # 3.57
\`\`\`

### Type Coercion

R automatically converts types in arithmetic:

\`\`\`r
cat(TRUE + 1, "\\n")   # 2 (TRUE becomes 1)
cat(FALSE + 1, "\\n")  # 1 (FALSE becomes 0)
\`\`\`

### Your Task

Calculate and print the following on separate lines:
1. \`17 modulo 5\`
2. \`2 to the power of 10\`
3. \`The square root of 144\``,

  starterCode: `# Calculate and print:
# 1. 17 modulo 5
# 2. 2 to the power of 10
# 3. The square root of 144
`,

  solution: `cat(17 %% 5, "\\n")
cat(2 ^ 10, "\\n")
cat(sqrt(144), "\\n")
`,

  tests: [
    {
      name: "prints arithmetic results",
      expected: "2 \n1024 \n12 \n",
    },
  ],
};
