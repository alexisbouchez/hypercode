import type { Lesson } from "../../types";

export const applyFunctions: Lesson = {
  id: "apply-functions",
  title: "Apply Functions",
  chapterId: "data-manipulation",
  content: `## The Apply Family

R's apply functions let you apply a function to elements of a data structure without writing explicit loops.

### \`sapply()\` -- Simplify Apply

Applies a function to each element of a vector and simplifies the result:

\`\`\`r
x <- c(1, 4, 9, 16, 25)
result <- sapply(x, sqrt)
cat(result, "\\n")  # 1 2 3 4 5
\`\`\`

### \`lapply()\` -- List Apply

Like \`sapply()\` but always returns a list:

\`\`\`r
x <- c(1, 4, 9)
result <- lapply(x, sqrt)
# result is a list: list(1, 2, 3)
\`\`\`

### \`vapply()\` -- Verified Apply

Like \`sapply()\` but you specify the expected return type for safety:

\`\`\`r
x <- c(1, 4, 9)
result <- vapply(x, sqrt, numeric(1))
cat(result, "\\n")  # 1 2 3
\`\`\`

### \`apply()\` -- For Matrices

Apply a function to rows or columns of a matrix:

\`\`\`r
m <- matrix(1:6, nrow = 2, byrow = TRUE)
cat(apply(m, 1, sum), "\\n")  # Row sums: 6 15
cat(apply(m, 2, sum), "\\n")  # Col sums: 5 7 9
\`\`\`

The second argument is the margin: \`1\` for rows, \`2\` for columns.

### With Anonymous Functions

\`\`\`r
result <- sapply(1:5, function(x) x ^ 2 + 1)
cat(result, "\\n")  # 2 5 10 17 26
\`\`\`

### Your Task

Use \`sapply()\` to compute the cube of each number from 1 to 5. Print the result.`,

  starterCode: `# Use sapply to compute cubes of 1 through 5
`,

  solution: `result <- sapply(1:5, function(x) x ^ 3)
cat(result, "\\n")
`,

  tests: [
    {
      name: "prints cubes",
      expected: "1 8 27 64 125 \n",
    },
  ],
};
