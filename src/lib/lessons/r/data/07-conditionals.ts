import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "control-flow",
  content: `## If / Else

R uses \`if\`, \`else if\`, and \`else\` for conditional execution:

\`\`\`r
x <- 10

if (x > 0) {
  cat("positive\\n")
} else if (x < 0) {
  cat("negative\\n")
} else {
  cat("zero\\n")
}
\`\`\`

The condition must be in parentheses. The braces are required for multi-line blocks and recommended even for single lines.

### Comparison Operators

| Operator | Description |
|----------|-------------|
| \`==\` | Equal to |
| \`!=\` | Not equal to |
| \`<\` | Less than |
| \`>\` | Greater than |
| \`<=\` | Less than or equal |
| \`>=\` | Greater than or equal |

### Logical Operators

| Operator | Description |
|----------|-------------|
| \`&&\` | AND (scalar) |
| \`\\|\\|\` | OR (scalar) |
| \`!\` | NOT |

Use \`&&\` and \`\\|\\|\` for scalar comparisons (in \`if\` statements). Use \`&\` and \`\\|\` for element-wise vector operations.

### The \`ifelse()\` Function

For vectorized conditional operations, use \`ifelse()\`:

\`\`\`r
x <- c(1, -2, 3, -4, 5)
result <- ifelse(x > 0, "pos", "neg")
cat(result, "\\n")  # pos neg pos neg pos
\`\`\`

### Your Task

Write a function \`classify\` that takes a number and returns \`"positive"\`, \`"negative"\`, or \`"zero"\`. Call it with \`5\`, \`-3\`, and \`0\`, printing each result.`,

  starterCode: `classify <- function(x) {
\t# Return "positive", "negative", or "zero"
}

cat(classify(5), "\\n")
cat(classify(-3), "\\n")
cat(classify(0), "\\n")
`,

  solution: `classify <- function(x) {
\tif (x > 0) {
\t\treturn("positive")
\t} else if (x < 0) {
\t\treturn("negative")
\t} else {
\t\treturn("zero")
\t}
}

cat(classify(5), "\\n")
cat(classify(-3), "\\n")
cat(classify(0), "\\n")
`,

  tests: [
    {
      name: "classifies numbers",
      expected: "positive \nnegative \nzero \n",
    },
  ],
};
