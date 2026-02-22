import type { Lesson } from "../../types";

export const defaultParams: Lesson = {
  id: "default-params",
  title: "Default Parameters",
  chapterId: "functions",
  content: `## Default and Named Parameters

Kotlin functions can have **default parameter values**:

\`\`\`kotlin
fun greet(name: String, greeting: String = "Hello"): String {
    return "$greeting, $name!"
}

println(greet("Alice"))          // Hello, Alice!
println(greet("Bob", "Hi"))      // Hi, Bob!
\`\`\`

You can also use **named arguments** to pass them in any order:

\`\`\`kotlin
println(greet(greeting = "Hey", name = "Carol"))  // Hey, Carol!
\`\`\`

Default parameters eliminate the need for overloaded functions.

## Your Turn

Write a function \`describe(item: String, color: String = "red", count: Int = 1): String\` that returns a string like \`"3 blue apples"\`.
`,
  starterCode: `fun describe(item: String, color: String = "red", count: Int = 1): String {
    return "$count $color \${item}s"
}

fun main() {
    println(describe("apple"))
    println(describe("car", "blue"))
    println(describe("book", "green", 5))
}
`,
  solution: `fun describe(item: String, color: String = "red", count: Int = 1): String {
    return "$count $color \${item}s"
}

fun main() {
    println(describe("apple"))
    println(describe("car", "blue"))
    println(describe("book", "green", 5))
}
`,
  tests: [
    {
      name: 'describe("apple") with defaults',
      code: '{{FUNC}}\nfun main() {\n    println(describe("apple"))\n}',
      expected: "1 red apples\n",
    },
    {
      name: 'describe("car", "blue")',
      code: '{{FUNC}}\nfun main() {\n    println(describe("car", "blue"))\n}',
      expected: "1 blue cars\n",
    },
    {
      name: 'describe("book", "green", 5)',
      code: '{{FUNC}}\nfun main() {\n    println(describe("book", "green", 5))\n}',
      expected: "5 green books\n",
    },
  ],
};
