import type { Lesson } from "../../types";

export const higherOrder: Lesson = {
  id: "higher-order",
  title: "Higher-Order Functions",
  chapterId: "oop",
  content: `## Higher-Order Functions

A **higher-order function** takes a function as a parameter or returns one. Function types look like \`(Int) -> Int\`:

\`\`\`kotlin
fun applyTwice(f: (Int) -> Int, x: Int): Int = f(f(x))

val double = { n: Int -> n * 2 }
println(applyTwice(double, 3))  // 12
\`\`\`

This enables powerful abstractions. You can chain list operations:

\`\`\`kotlin
val result = listOf(1, 2, 3, 4, 5)
    .filter { it % 2 != 0 }
    .map { it * it }
// [1, 9, 25]
\`\`\`

Higher-order functions are at the heart of functional programming.

## Your Turn

Write a function \`applyTwice(f: (Int) -> Int, x: Int): Int\` that applies \`f\` to \`x\` twice. Then use it with a tripling function to compute \`applyTwice(triple, 2)\` and \`applyTwice(triple, 4)\`.
`,
  starterCode: `fun applyTwice(f: (Int) -> Int, x: Int): Int = f(f(x))

fun main() {
    val triple = { n: Int -> n * 3 }
    println(applyTwice(triple, 2))
    println(applyTwice(triple, 4))

    val nums = listOf(1, 2, 3, 4, 5)
    val result = nums.filter { it % 2 != 0 }.map { it * it }
    println(result)
}
`,
  solution: `fun applyTwice(f: (Int) -> Int, x: Int): Int = f(f(x))

fun main() {
    val triple = { n: Int -> n * 3 }
    println(applyTwice(triple, 2))
    println(applyTwice(triple, 4))

    val nums = listOf(1, 2, 3, 4, 5)
    val result = nums.filter { it % 2 != 0 }.map { it * it }
    println(result)
}
`,
  tests: [
    {
      name: "applyTwice(triple, 2) = 18",
      code: "{{FUNC}}\nfun main() {\n    val triple = { n: Int -> n * 3 }\n    println(applyTwice(triple, 2))\n}",
      expected: "18\n",
    },
    {
      name: "applyTwice(triple, 4) = 36",
      code: "{{FUNC}}\nfun main() {\n    val triple = { n: Int -> n * 3 }\n    println(applyTwice(triple, 4))\n}",
      expected: "36\n",
    },
    {
      name: "filter odds and square",
      expected: "18\n36\n[1, 9, 25]\n",
    },
  ],
};
