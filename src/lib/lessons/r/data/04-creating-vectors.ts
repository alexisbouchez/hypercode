import type { Lesson } from "../../types";

export const creatingVectors: Lesson = {
  id: "creating-vectors",
  title: "Creating Vectors",
  chapterId: "vectors",
  content: `## Vectors

Vectors are the most fundamental data structure in R. A vector is an ordered collection of elements of the same type.

### Creating Vectors with \`c()\`

The \`c()\` function combines values into a vector:

\`\`\`r
nums <- c(1, 2, 3, 4, 5)
names <- c("Alice", "Bob", "Charlie")
flags <- c(TRUE, FALSE, TRUE)
\`\`\`

### Sequences

Use the \`:\` operator for integer sequences:

\`\`\`r
x <- 1:5   # c(1, 2, 3, 4, 5)
y <- 3:7   # c(3, 4, 5, 6, 7)
\`\`\`

Use \`seq()\` for more control:

\`\`\`r
seq(1, 10, by = 2)       # c(1, 3, 5, 7, 9)
seq(0, 1, length.out = 5) # c(0.00, 0.25, 0.50, 0.75, 1.00)
\`\`\`

### Repetition

Use \`rep()\` to repeat values:

\`\`\`r
rep(0, 5)          # c(0, 0, 0, 0, 0)
rep(c(1, 2), 3)    # c(1, 2, 1, 2, 1, 2)
rep(c(1, 2), each = 3) # c(1, 1, 1, 2, 2, 2)
\`\`\`

### Printing Vectors

Use \`cat()\` with vectors -- it prints elements separated by spaces:

\`\`\`r
x <- c(10, 20, 30)
cat(x, "\\n")  # 10 20 30
\`\`\`

### Your Task

Create a vector containing the even numbers from 2 to 10, and print it.`,

  starterCode: `# Create a vector of even numbers from 2 to 10 and print it
`,

  solution: `x <- seq(2, 10, by = 2)
cat(x, "\\n")
`,

  tests: [
    {
      name: "prints even numbers",
      expected: "2 4 6 8 10 \n",
    },
  ],
};
