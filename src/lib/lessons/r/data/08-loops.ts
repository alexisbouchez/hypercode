import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## For Loops

R's \`for\` loop iterates over elements of a vector:

\`\`\`r
for (i in 1:5) {
  cat(i, "\\n")
}
\`\`\`

You can iterate over any vector:

\`\`\`r
fruits <- c("apple", "banana", "cherry")
for (fruit in fruits) {
  cat(fruit, "\\n")
}
\`\`\`

### While Loops

The \`while\` loop runs while a condition is true:

\`\`\`r
x <- 1
while (x <= 5) {
  cat(x, "\\n")
  x <- x + 1
}
\`\`\`

### Loop Control

Use \`next\` to skip to the next iteration, and \`break\` to exit the loop:

\`\`\`r
for (i in 1:10) {
  if (i %% 2 == 0) next  # Skip even numbers
  if (i > 7) break       # Stop after 7
  cat(i, "\\n")
}
# Prints: 1 3 5 7
\`\`\`

### A Note on Vectorization

In R, you should prefer vectorized operations over loops when possible. Loops are fine for learning, but R is optimized for vector operations:

\`\`\`r
# Slow (loop)
result <- c()
for (i in 1:5) result <- c(result, i ^ 2)

# Fast (vectorized)
result <- (1:5) ^ 2
\`\`\`

### Your Task

Use a \`for\` loop to print the first 7 numbers of the Fibonacci sequence (1, 1, 2, 3, 5, 8, 13), each on its own line.`,

  starterCode: `# Print the first 7 Fibonacci numbers, one per line
`,

  solution: `a <- 1
b <- 1
for (i in 1:7) {
\tcat(a, "\\n")
\ttemp <- a
\ta <- b
\tb <- temp + b
}
`,

  tests: [
    {
      name: "prints Fibonacci sequence",
      expected: "1 \n1 \n2 \n3 \n5 \n8 \n13 \n",
    },
  ],
};
