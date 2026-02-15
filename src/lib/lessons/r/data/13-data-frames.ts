import type { Lesson } from "../../types";

export const dataFrames: Lesson = {
  id: "data-frames",
  title: "Data Frames",
  chapterId: "data-structures",
  content: `## Data Frames

A data frame is R's primary structure for tabular data. Each column is a vector, and columns can have different types:

\`\`\`r
df <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 35),
  active = c(TRUE, FALSE, TRUE)
)
\`\`\`

### Accessing Columns

Use \`$\` to access a column by name:

\`\`\`r
cat(df$name, "\\n")   # Alice Bob Charlie
cat(df$age, "\\n")    # 25 30 35
\`\`\`

### Dimensions

\`\`\`r
cat(nrow(df), "\\n")   # 3 (number of rows)
cat(ncol(df), "\\n")   # 3 (number of columns)
\`\`\`

### Indexing

Use \`[row, col]\` notation, similar to matrices:

\`\`\`r
cat(df[1, 2], "\\n")      # 25 (row 1, col 2)
cat(df[2, "name"], "\\n") # Bob (row 2, column "name")
\`\`\`

### Adding Columns

\`\`\`r
df$score <- c(90, 85, 92)  # Add a new column
\`\`\`

### Filtering Rows

\`\`\`r
young <- df[df$age < 30, ]
# Returns rows where age < 30
\`\`\`

### Your Task

Create a data frame with columns \`language\` (\`"R"\`, \`"Python"\`, \`"Julia"\`) and \`year\` (\`1993\`, \`1991\`, \`2012\`). Print the \`language\` column, then print the number of rows.`,

  starterCode: `# Create a data frame and print its language column and row count
`,

  solution: `df <- data.frame(
\tlanguage = c("R", "Python", "Julia"),
\tyear = c(1993, 1991, 2012)
)
cat(df$language, "\\n")
cat(nrow(df), "\\n")
`,

  tests: [
    {
      name: "prints languages and row count",
      expected: "R Python Julia \n3 \n",
    },
  ],
};
