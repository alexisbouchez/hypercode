import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "data-structures",
  content: `## Lists

Unlike vectors, lists can hold elements of different types:

\`\`\`r
person <- list(name = "Alice", age = 30, active = TRUE)
\`\`\`

### Accessing Elements

There are three ways to access list elements:

\`\`\`r
person <- list(name = "Alice", age = 30)

# By name with $
cat(person$name, "\\n")      # Alice

# By name with [[]]
cat(person[["age"]], "\\n")   # 30

# By position with [[]]
cat(person[[1]], "\\n")       # Alice
\`\`\`

Note the double brackets \`[[]]\` -- single brackets \`[]\` return a sub-list, not the element itself.

### Modifying Lists

You can add or modify elements:

\`\`\`r
person <- list(name = "Alice", age = 30)
person$email <- "alice@example.com"  # Add element
person$age <- 31                      # Modify element
\`\`\`

### Nested Lists

Lists can contain other lists:

\`\`\`r
team <- list(
  lead = list(name = "Alice", role = "Lead"),
  dev = list(name = "Bob", role = "Developer")
)
cat(team$lead$name, "\\n")  # Alice
\`\`\`

### Your Task

Create a list called \`book\` with elements \`title\` (\`"The R Book"\`), \`pages\` (\`1060\`), and \`available\` (\`TRUE\`). Print each element on its own line using \`$\` notation.`,

  starterCode: `# Create a list and print its elements
`,

  solution: `book <- list(title = "The R Book", pages = 1060, available = TRUE)
cat(book$title, "\\n")
cat(book$pages, "\\n")
cat(book$available, "\\n")
`,

  tests: [
    {
      name: "prints list elements",
      expected: "The R Book \n1060 \nTRUE \n",
    },
  ],
};
