import type { Lesson } from "../../types";

export const nullSafety: Lesson = {
  id: "null-safety",
  title: "Null Safety",
  chapterId: "oop",
  content: `## Null Safety

Kotlin's type system distinguishes between **nullable** and **non-nullable** types. By default, variables cannot hold \`null\`:

\`\`\`kotlin
var name: String = "Alice"
name = null  // Compile error!
\`\`\`

To allow null, add \`?\` to the type:

\`\`\`kotlin
var name: String? = "Alice"
name = null  // OK
\`\`\`

### Safe Calls (?.)

The safe call operator \`?.\` calls a method only if the value is not null. If it is null, the whole expression returns \`null\`:

\`\`\`kotlin
val name: String? = "HELLO"
println(name?.lowercase())  // hello

val empty: String? = null
println(empty?.lowercase()) // null
\`\`\`

### Elvis Operator (?:)

The Elvis operator \`?:\` provides a default value when the left side is null:

\`\`\`kotlin
val name: String? = null
val result = name ?: "stranger"
println(result) // stranger
\`\`\`

### Non-Null Assertion (!!)

The \`!!\` operator asserts that a value is not null. If it is null at runtime, it throws an exception:

\`\`\`kotlin
val name: String? = "Hello"
println(name!!.length) // 5
\`\`\`

Use \`!!\` sparingly — prefer safe calls or null checks instead.

### Smart Casts

When you check for null with \`if\`, Kotlin smart-casts the variable to non-nullable inside the block:

\`\`\`kotlin
val name: String? = "Alice"
if (name != null) {
    println(name.length) // Smart cast: name is String here
}
\`\`\`

### The let Scope Function

Use \`?.let { }\` to execute a block only when a value is non-null. Inside the block, the value is available as \`it\`:

\`\`\`kotlin
val name: String? = "Alice"
name?.let { println("Hello, $it") } // Hello, Alice

val empty: String? = null
empty?.let { println("Hello, $it") } // not executed
\`\`\`

## Your Turn

Write a function \`greet\` that takes a nullable \`String?\` parameter. If the name is not null, print \`"Hello, <name>!"\` using the Elvis operator to provide \`"World"\` as a default. Then in \`main\`, call it with \`"Alice"\`, \`null\`, and \`"Bob"\`.
`,
  starterCode: `fun greet(name: String?) {
    val displayName = name ?: "World"
    println("Hello, $displayName!")
}

fun main() {
    greet("Alice")
    greet(null)
    greet("Bob")
}
`,
  solution: `fun greet(name: String?) {
    val displayName = name ?: "World"
    println("Hello, $displayName!")
}

fun main() {
    greet("Alice")
    greet(null)
    greet("Bob")
}
`,
  tests: [
    {
      name: "greet with values and null",
      expected: "Hello, Alice!\nHello, World!\nHello, Bob!\n",
    },
    {
      name: "safe call on nullable",
      expected: "hello\nnull\n",
      code: `{{FUNC}}
fun main() {
    val s: String? = "HELLO"
    println(s?.lowercase())
    val n: String? = null
    println(n?.lowercase())
}
`,
    },
    {
      name: "non-null assertion",
      expected: "5\n",
      code: `{{FUNC}}
fun main() {
    val s: String? = "Hello"
    println(s!!.length)
}
`,
    },
    {
      name: "let scope function",
      expected: "Name is Alice\ndone\n",
      code: `{{FUNC}}
fun main() {
    val name: String? = "Alice"
    name?.let { println("Name is $it") }
    val empty: String? = null
    empty?.let { println("Name is $it") }
    println("done")
}
`,
    },
  ],
};
