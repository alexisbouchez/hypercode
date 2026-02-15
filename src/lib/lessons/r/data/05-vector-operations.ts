import type { Lesson } from "../../types";

export const vectorOperations: Lesson = {
  id: "vector-operations",
  title: "Vector Operations",
  chapterId: "vectors",
  content: `## Vectorized Operations

R operates on entire vectors at once -- no loops needed:

\`\`\`r
x <- c(1, 2, 3, 4, 5)
cat(x * 2, "\\n")     # 2 4 6 8 10
cat(x + 10, "\\n")    # 11 12 13 14 15
cat(x ^ 2, "\\n")     # 1 4 9 16 25
\`\`\`

### Element-wise Operations

When two vectors have the same length, operations are element-wise:

\`\`\`r
a <- c(1, 2, 3)
b <- c(10, 20, 30)
cat(a + b, "\\n")  # 11 22 33
cat(a * b, "\\n")  # 10 40 90
\`\`\`

### Summary Functions

R has built-in functions for summarizing vectors:

| Function | Description |
|----------|-------------|
| \`sum()\` | Sum of all elements |
| \`mean()\` | Arithmetic mean |
| \`min()\` | Minimum value |
| \`max()\` | Maximum value |
| \`length()\` | Number of elements |
| \`range()\` | Min and max values |

\`\`\`r
x <- c(4, 8, 15, 16, 23, 42)
cat(sum(x), "\\n")    # 108
cat(mean(x), "\\n")   # 18
cat(min(x), "\\n")    # 4
cat(max(x), "\\n")    # 42
cat(length(x), "\\n") # 6
\`\`\`

### Your Task

Create a vector with the values \`3, 7, 1, 9, 4\`. Print its sum and its mean on separate lines.`,

  starterCode: `# Create a vector and print its sum and mean
`,

  solution: `x <- c(3, 7, 1, 9, 4)
cat(sum(x), "\\n")
cat(mean(x), "\\n")
`,

  tests: [
    {
      name: "prints sum and mean",
      expected: "24 \n4.8 \n",
    },
  ],
};
