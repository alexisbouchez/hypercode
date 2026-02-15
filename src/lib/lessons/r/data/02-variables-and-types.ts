import type { Lesson } from "../../types";

export const variablesAndTypes: Lesson = {
  id: "variables-and-types",
  title: "Variables and Types",
  chapterId: "foundations",
  content: `## Variables

In R, you assign values to variables using the \`<-\` operator:

\`\`\`r
x <- 42
name <- "Alice"
is_active <- TRUE
\`\`\`

You can also use \`=\` for assignment, but \`<-\` is the conventional R style.

### Basic Types

R has several fundamental types:

| Type | Example | Description |
|------|---------|-------------|
| numeric | \`3.14\` | Numbers (both integers and doubles) |
| character | \`"hello"\` | Strings (text) |
| logical | \`TRUE\`, \`FALSE\` | Boolean values |
| integer | \`42L\` | Explicit integer (note the \`L\` suffix) |

### Checking Types

Use \`class()\` to check the type of a value:

\`\`\`r
cat(class(42), "\\n")       # "numeric"
cat(class("hello"), "\\n")  # "character"
cat(class(TRUE), "\\n")     # "logical"
\`\`\`

### String Concatenation

Use \`paste()\` or \`paste0()\` to combine strings:

\`\`\`r
name <- "World"
cat(paste("Hello,", name), "\\n")   # "Hello, World" (space separator)
cat(paste0("Hello, ", name), "\\n") # "Hello, World" (no separator)
\`\`\`

\`paste()\` adds a space between arguments by default. \`paste0()\` concatenates without any separator.

### Your Task

Create three variables: \`name\` (set to \`"R"\`), \`year\` (set to \`1993\`), and \`is_free\` (set to \`TRUE\`). Print each on its own line using \`cat()\`.`,

  starterCode: `# Create three variables and print them
`,

  solution: `name <- "R"
year <- 1993
is_free <- TRUE
cat(name, "\\n")
cat(year, "\\n")
cat(is_free, "\\n")
`,

  tests: [
    {
      name: "prints variables",
      expected: "R \n1993 \nTRUE \n",
    },
  ],
};
