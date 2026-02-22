import type { Lesson } from "../../types";

export const lambdas: Lesson = {
  id: "lambdas",
  title: "Lambdas",
  chapterId: "functions",
  content: `## Lambda Expressions

Lambdas are anonymous functions you can store in variables or pass to other functions:

\`\`\`kotlin
val double = { x: Int -> x * 2 }
println(double(5))   // 10
\`\`\`

The syntax is \`{ parameters -> body }\`. For multiple parameters:

\`\`\`kotlin
val add = { x: Int, y: Int -> x + y }
println(add(3, 4))   // 7
\`\`\`

Lambdas can be passed to functions that expect a function type:

\`\`\`kotlin
val nums = listOf(1, 2, 3)
val doubled = nums.map { it * 2 }  // [2, 4, 6]
\`\`\`

When there's only one parameter, Kotlin provides the implicit name \`it\`.

## Your Turn

Create a lambda \`square\` that squares a number, and a lambda \`isEven\` that returns \`true\` if a number is even. Apply them to a list.
`,
  starterCode: `fun main() {
    val square = { x: Int -> x * x }
    val isEven = { x: Int -> x % 2 == 0 }

    println(square(4))
    println(square(7))
    println(isEven(6))
    println(isEven(7))
}
`,
  solution: `fun main() {
    val square = { x: Int -> x * x }
    val isEven = { x: Int -> x % 2 == 0 }

    println(square(4))
    println(square(7))
    println(isEven(6))
    println(isEven(7))
}
`,
  tests: [
    {
      name: "square and isEven",
      expected: "16\n49\ntrue\nfalse\n",
    },
  ],
};
