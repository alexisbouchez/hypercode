import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions in Kotlin

Functions are declared with \`fun\`:

\`\`\`kotlin
fun add(x: Int, y: Int): Int {
    return x + y
}
\`\`\`

For single-expression functions, you can use the concise \`=\` syntax:

\`\`\`kotlin
fun add(x: Int, y: Int): Int = x + y
fun square(n: Int): Int = n * n
\`\`\`

Call functions normally:

\`\`\`kotlin
println(add(3, 4))    // 7
println(square(5))    // 25
\`\`\`

## Your Turn

Write a function \`cube(n: Int): Int\` that returns \`n\` cubed, and a function \`sumOfCubes(a: Int, b: Int): Int\` that returns the sum of the cubes of \`a\` and \`b\`.
`,
  starterCode: `fun cube(n: Int): Int = n * n * n

fun sumOfCubes(a: Int, b: Int): Int = cube(a) + cube(b)

fun main() {
    println(cube(3))
    println(cube(4))
    println(sumOfCubes(2, 3))
}
`,
  solution: `fun cube(n: Int): Int = n * n * n

fun sumOfCubes(a: Int, b: Int): Int = cube(a) + cube(b)

fun main() {
    println(cube(3))
    println(cube(4))
    println(sumOfCubes(2, 3))
}
`,
  tests: [
    {
      name: "cube(3) = 27",
      code: "{{FUNC}}\nfun main() {\n    println(cube(3))\n}",
      expected: "27\n",
    },
    {
      name: "cube(4) = 64",
      code: "{{FUNC}}\nfun main() {\n    println(cube(4))\n}",
      expected: "64\n",
    },
    {
      name: "sumOfCubes(2, 3) = 35",
      code: "{{FUNC}}\nfun main() {\n    println(sumOfCubes(2, 3))\n}",
      expected: "35\n",
    },
  ],
};
