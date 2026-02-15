import type { Lesson } from "../../types";

export const stringOperations: Lesson = {
  id: "string-operations",
  title: "String Operations",
  chapterId: "strings",
  content: `## String Functions

R has many built-in string manipulation functions:

### Length and Case

\`\`\`r
cat(nchar("hello"), "\\n")       # 5
cat(toupper("hello"), "\\n")     # HELLO
cat(tolower("HELLO"), "\\n")     # hello
\`\`\`

### Concatenation

\`\`\`r
cat(paste("hello", "world"), "\\n")       # hello world
cat(paste0("hello", "world"), "\\n")      # helloworld
cat(paste("a", "b", "c", sep = "-"), "\\n") # a-b-c
\`\`\`

### Substring

\`\`\`r
x <- "Hello, World!"
cat(substring(x, 1, 5), "\\n")  # Hello
cat(substring(x, 8), "\\n")     # World!
\`\`\`

### Search and Replace

\`\`\`r
x <- "Hello, World!"
cat(gsub("World", "R", x), "\\n")       # Hello, R!
cat(grepl("World", x), "\\n")           # TRUE
\`\`\`

\`gsub()\` replaces all matches. \`sub()\` replaces only the first match. \`grepl()\` returns \`TRUE\` or \`FALSE\`.

### Splitting Strings

\`\`\`r
parts <- strsplit("a,b,c", ",")[[1]]
cat(parts, "\\n")  # a b c
\`\`\`

Note: \`strsplit()\` returns a list, so use \`[[1]]\` to get the character vector.

### Formatting

\`\`\`r
cat(sprintf("Name: %s, Age: %d", "Alice", 30), "\\n")
cat(sprintf("Pi is %.2f", 3.14159), "\\n")
\`\`\`

### Your Task

Take the string \`"hello, world"\`, convert it to uppercase, replace \`"WORLD"\` with \`"R"\`, and print the result.`,

  starterCode: `# Transform "hello, world" -> uppercase -> replace WORLD with R -> print
`,

  solution: `x <- "hello, world"
x <- toupper(x)
x <- gsub("WORLD", "R", x)
cat(x, "\\n")
`,

  tests: [
    {
      name: "transforms string",
      expected: "HELLO, R \n",
    },
  ],
};
