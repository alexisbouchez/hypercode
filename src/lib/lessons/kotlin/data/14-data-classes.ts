import type { Lesson } from "../../types";

export const dataClasses: Lesson = {
  id: "data-classes",
  title: "Data Classes",
  chapterId: "oop",
  content: `## Data Classes

A \`data class\` is a class whose primary purpose is to hold data. Kotlin automatically generates \`toString\`, \`equals\`, \`copy\`, and more:

\`\`\`kotlin
data class Point(val x: Int, val y: Int)

val p = Point(3, 4)
println(p)           // Point(x=3, y=4)
println(p.x)         // 3
\`\`\`

The \`copy\` function creates a new instance with some values changed:

\`\`\`kotlin
val q = p.copy(x = 10)
println(q)           // Point(x=10, y=4)
\`\`\`

Data classes are perfect for representing immutable values like coordinates, records, or configuration.

## Your Turn

Define a \`data class Person(val name: String, val age: Int)\`. Create a person named \`"Alice"\` aged \`30\`, print them, then create a copy with age \`31\` and print that.
`,
  starterCode: `data class Person(val name: String, val age: Int)

fun main() {
    val alice = Person("Alice", 30)
    println(alice)
    println(alice.name)
    val older = alice.copy(age = 31)
    println(older)
}
`,
  solution: `data class Person(val name: String, val age: Int)

fun main() {
    val alice = Person("Alice", 30)
    println(alice)
    println(alice.name)
    val older = alice.copy(age = 31)
    println(older)
}
`,
  tests: [
    {
      name: "data class Person",
      expected: "Person(name=Alice, age=30)\nAlice\nPerson(name=Alice, age=31)\n",
    },
  ],
};
