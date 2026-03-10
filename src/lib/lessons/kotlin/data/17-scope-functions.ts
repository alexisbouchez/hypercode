import type { Lesson } from "../../types";

export const scopeFunctions: Lesson = {
  id: "scope-functions",
  title: "Scope Functions & Extensions",
  chapterId: "oop",
  content: `## Scope Functions & Extension Functions

Kotlin provides five **scope functions** that execute a block of code on an object: \`let\`, \`also\`, \`apply\`, \`run\`, and \`with\`. They differ in how they refer to the object and what they return.

### let

\`let\` passes the object as \`it\` and returns the **lambda result**. Often used with safe calls:

\`\`\`kotlin
val name: String? = "Alice"
val len = name?.let { it.length }
println(len) // 5
\`\`\`

### also

\`also\` passes the object as \`it\` and returns the **object itself**. Great for side effects like logging:

\`\`\`kotlin
val numbers = mutableListOf(1, 2, 3)
numbers.also { println("Before: $it") }
       .add(4)
println(numbers) // [1, 2, 3, 4]
\`\`\`

### apply

\`apply\` gives access to the object as \`it\` and returns the **object itself**. Useful for configuring objects:

\`\`\`kotlin
val result = mutableListOf(1, 2).apply { it.add(3) }
println(result) // [1, 2, 3]
\`\`\`

### run

\`run\` gives access to the object as \`it\` and returns the **lambda result**:

\`\`\`kotlin
val greeting = "Hello".run { "$it, World!" }
println(greeting) // Hello, World!
\`\`\`

### with

\`with\` is not an extension — you pass the object as an argument. It returns the **lambda result**:

\`\`\`kotlin
val numbers = listOf(1, 2, 3)
val summary = with(numbers) { "Count: $\{it.size\}" }
println(summary) // Count: 3
\`\`\`

### Extension Functions

Extension functions let you add new methods to existing types without modifying them:

\`\`\`kotlin
fun String.exclaim(): String = this + "!"
fun String.repeat3(): String = this + this + this

println("Hello".exclaim()) // Hello!
println("ha".repeat3())    // hahaha
\`\`\`

## Your Turn

1. Write an extension function \`String.shout()\` that returns the string uppercased with \`"!"\` appended.
2. In \`main\`, use \`let\` to transform a string, \`also\` for a side effect, and \`with\` to build a summary.
`,
  starterCode: `fun String.shout(): String = this.uppercase() + "!"

fun main() {
    val name = "hello"

    val shouted = name.shout()
    println(shouted)

    val result = name.let { it.uppercase() }
    println(result)

    name.also { println("Original: $it") }

    val summary = with(listOf(1, 2, 3)) { "Size: $\{it.size\}" }
    println(summary)
}
`,
  solution: `fun String.shout(): String = this.uppercase() + "!"

fun main() {
    val name = "hello"

    val shouted = name.shout()
    println(shouted)

    val result = name.let { it.uppercase() }
    println(result)

    name.also { println("Original: $it") }

    val summary = with(listOf(1, 2, 3)) { "Size: $\{it.size\}" }
    println(summary)
}
`,
  tests: [
    {
      name: "shout extension and let",
      expected: "HELLO!\nHELLO\nOriginal: hello\nSize: 3\n",
    },
    {
      name: "extension function on String",
      expected: "WOW!\nCOOL!\n",
      code: `fun String.shout(): String = this.uppercase() + "!"

fun main() {
    println("wow".shout())
    println("cool".shout())
}
`,
    },
    {
      name: "also returns the original object",
      expected: "side\n5\n",
      code: `fun main() {
    val x = "hello".also { println("side") }
    println(x.length)
}
`,
    },
    {
      name: "with scope function",
      expected: "Items: 3\n",
      code: `fun main() {
    val nums = listOf(10, 20, 30)
    val info = with(nums) { "Items: $\{it.size\}" }
    println(info)
}
`,
    },
  ],
};
