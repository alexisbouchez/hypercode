import type { Lesson } from "../../types";

export const matrices: Lesson = {
  id: "matrices",
  title: "Matrices",
  chapterId: "data-structures",
  content: `## Matrices

A matrix is a 2D array of elements, all of the same type. Create one with \`matrix()\`:

\`\`\`r
m <- matrix(1:6, nrow = 2, ncol = 3)
# Creates:
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
\`\`\`

By default, R fills matrices **column by column**. Use \`byrow = TRUE\` to fill by row:

\`\`\`r
m <- matrix(1:6, nrow = 2, byrow = TRUE)
# Creates:
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6
\`\`\`

### Indexing

Use \`[row, col]\` to access elements:

\`\`\`r
m <- matrix(1:9, nrow = 3, byrow = TRUE)
cat(m[1, 2], "\\n")  # Row 1, Col 2 -> 2
cat(m[2, ], "\\n")   # Entire row 2 -> 4 5 6
cat(m[, 3], "\\n")   # Entire col 3 -> 3 6 9
\`\`\`

### Matrix Operations

\`\`\`r
m <- matrix(1:4, nrow = 2)
cat(nrow(m), "\\n")  # 2
cat(ncol(m), "\\n")  # 2
cat(t(m), "\\n")     # Transpose (flattened output)
\`\`\`

Arithmetic works element-wise, just like vectors:

\`\`\`r
m <- matrix(1:4, nrow = 2)
cat(m * 2, "\\n")  # 2 4 6 8
\`\`\`

### Your Task

Create a 3x3 matrix filled by row with the values 1 through 9. Print the element at row 2, column 3, then print the sum of all elements.`,

  starterCode: `# Create a 3x3 matrix and print element [2,3] and the total sum
`,

  solution: `m <- matrix(1:9, nrow = 3, byrow = TRUE)
cat(m[2, 3], "\\n")
cat(sum(m), "\\n")
`,

  tests: [
    {
      name: "prints matrix element and sum",
      expected: "6 \n45 \n",
    },
  ],
};
