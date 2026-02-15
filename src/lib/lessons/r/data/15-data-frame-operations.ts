import type { Lesson } from "../../types";

export const dataFrameOperations: Lesson = {
  id: "data-frame-operations",
  title: "Data Frame Operations",
  chapterId: "data-manipulation",
  content: `## Filtering with \`subset()\`

The \`subset()\` function filters rows and selects columns:

\`\`\`r
df <- data.frame(
  name = c("Alice", "Bob", "Charlie", "Diana"),
  age = c(25, 30, 35, 28),
  score = c(90, 85, 92, 88)
)

young <- subset(df, age < 30)
# Returns rows where age < 30
\`\`\`

You can also select specific columns:

\`\`\`r
result <- subset(df, age >= 30, select = c(name, score))
\`\`\`

### Sorting with \`order()\`

Use \`order()\` to sort a data frame:

\`\`\`r
sorted <- df[order(df$age), ]        # Sort by age ascending
sorted <- df[order(-df$age), ]       # Sort by age descending
sorted <- df[order(df$age, df$name), ] # Sort by age, then name
\`\`\`

### Merging Data Frames

Use \`merge()\` to join two data frames:

\`\`\`r
df1 <- data.frame(id = 1:3, name = c("A", "B", "C"))
df2 <- data.frame(id = 2:4, score = c(85, 92, 78))

merged <- merge(df1, df2, by = "id")
# Only matching ids (inner join by default)
\`\`\`

### Your Task

Create a data frame with columns \`city\` (\`"Paris"\`, \`"Tokyo"\`, \`"London"\`, \`"Sydney"\`) and \`pop\` (\`2.1\`, \`13.9\`, \`8.9\`, \`5.3\`). Sort it by \`pop\` in descending order and print the city column of the sorted result.`,

  starterCode: `# Create a data frame, sort by pop descending, print cities
`,

  solution: `df <- data.frame(
\tcity = c("Paris", "Tokyo", "London", "Sydney"),
\tpop = c(2.1, 13.9, 8.9, 5.3)
)
sorted <- df[order(-df$pop), ]
cat(sorted$city, "\\n")
`,

  tests: [
    {
      name: "prints cities sorted by population",
      expected: "Tokyo London Sydney Paris \n",
    },
  ],
};
