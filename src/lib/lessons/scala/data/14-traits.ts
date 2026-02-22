import type { Lesson } from "../../types";

export const traits: Lesson = {
  id: "traits",
  title: "Traits",
  chapterId: "oop",
  content: `## Traits

Traits define interfaces (and can include default implementations). Classes \`extend\` them:

\`\`\`scala
trait Greetable {
  def greet: String
}

class Person(name: String) extends Greetable {
  def greet: String = s"Hello, I'm $name"
}

val p = new Person("Alice")
println(p.greet)  // Hello, I'm Alice
\`\`\`

### Default Implementations

\`\`\`scala
trait Named {
  def name: String
  def shout: String = name.toUpperCase
}
\`\`\`

### Your Task

Define a trait \`Shape\` with an abstract method \`area: Double\`. Then define a \`class Circle(radius: Double) extends Shape\` that implements \`area\` as \`3.14159 * radius * radius\`.`,

  starterCode: `trait Shape {
  def area: Double
}

class Circle(radius: Double) extends Shape {
  def area: Double = 3.14159 * radius * radius
}

val c = new Circle(5.0)
println(c.area)
`,

  solution: `trait Shape {
  def area: Double
}

class Circle(radius: Double) extends Shape {
  def area: Double = 3.14159 * radius * radius
}

val c = new Circle(5.0)
println(c.area)
`,

  tests: [
    {
      name: "Circle(5.0).area ≈ 78.53975",
      expected: "78.53975\n",
      code: `{{FUNC}}
val c = new Circle(5.0)
println(c.area)
`,
    },
    {
      name: "Circle(1.0).area ≈ 3.14159",
      expected: "3.14159\n",
      code: `{{FUNC}}
val c = new Circle(1.0)
println(c.area)
`,
    },
  ],
};
