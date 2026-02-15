import type { Lesson } from "../../types";

export const indexingAndFiltering: Lesson = {
  id: "indexing-and-filtering",
  title: "Indexing and Filtering",
  chapterId: "vectors",
  content: `## Vector Indexing

R uses **1-based indexing** -- the first element is at position 1, not 0:

\`\`\`r
x <- c(10, 20, 30, 40, 50)
cat(x[1], "\\n")  # 10
cat(x[3], "\\n")  # 30
cat(x[5], "\\n")  # 50
\`\`\`

### Selecting Multiple Elements

Pass a vector of indices to select multiple elements:

\`\`\`r
x <- c(10, 20, 30, 40, 50)
cat(x[c(1, 3, 5)], "\\n")  # 10 30 50
cat(x[2:4], "\\n")          # 20 30 40
\`\`\`

### Negative Indexing

Use negative indices to exclude elements:

\`\`\`r
x <- c(10, 20, 30, 40, 50)
cat(x[-1], "\\n")       # 20 30 40 50 (exclude first)
cat(x[-c(2, 4)], "\\n") # 10 30 50 (exclude 2nd and 4th)
\`\`\`

### Logical Filtering

Use a logical vector to filter elements:

\`\`\`r
x <- c(3, 7, 1, 9, 4)
cat(x[x > 4], "\\n")    # 7 9
cat(x[x %% 2 == 1], "\\n") # 3 7 1 9 (odd numbers)
\`\`\`

The expression \`x > 4\` creates a logical vector \`c(FALSE, TRUE, FALSE, TRUE, FALSE)\`, which is then used to select elements.

### Your Task

Create a vector with the values \`12, 5, 8, 19, 3, 15\`. Use logical filtering to print only the values greater than 10.`,

  starterCode: `# Create a vector and print values greater than 10
`,

  solution: `x <- c(12, 5, 8, 19, 3, 15)
cat(x[x > 10], "\\n")
`,

  tests: [
    {
      name: "prints values greater than 10",
      expected: "12 19 15 \n",
    },
  ],
};
