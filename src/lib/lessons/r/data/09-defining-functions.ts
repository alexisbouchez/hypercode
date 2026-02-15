import type { Lesson } from "../../types";

export const definingFunctions: Lesson = {
  id: "defining-functions",
  title: "Defining Functions",
  chapterId: "functions",
  content: `## Functions in R

Functions are created with the \`function\` keyword:

\`\`\`r
square <- function(x) {
  return(x ^ 2)
}

cat(square(5), "\\n")  # 25
\`\`\`

### Implicit Return

In R, the last expression in a function is automatically returned. You can omit \`return()\`:

\`\`\`r
square <- function(x) {
  x ^ 2
}
\`\`\`

However, explicit \`return()\` is clearer, especially in functions with multiple branches.

### Default Arguments

You can set default values for parameters:

\`\`\`r
greet <- function(name, greeting = "Hello") {
  cat(paste(greeting, name), "\\n")
}

greet("Alice")            # Hello Alice
greet("Bob", "Hi")        # Hi Bob
\`\`\`

### Multiple Parameters

\`\`\`r
add <- function(a, b) {
  a + b
}

cat(add(3, 4), "\\n")  # 7
\`\`\`

### Your Task

Write a function \`factorial\` that computes the factorial of a number using a loop (not recursion). Print \`factorial(0)\`, \`factorial(5)\`, and \`factorial(10)\`.`,

  starterCode: `factorial <- function(n) {
\t# Compute n! using a loop
}

cat(factorial(0), "\\n")
cat(factorial(5), "\\n")
cat(factorial(10), "\\n")
`,

  solution: `factorial <- function(n) {
\tresult <- 1
\tif (n > 0) {
\t\tfor (i in 1:n) {
\t\t\tresult <- result * i
\t\t}
\t}
\treturn(result)
}

cat(factorial(0), "\\n")
cat(factorial(5), "\\n")
cat(factorial(10), "\\n")
`,

  tests: [
    {
      name: "computes factorials",
      expected: "1 \n120 \n3628800 \n",
    },
  ],
};
