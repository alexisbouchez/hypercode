import type { Lesson } from "../../types";

export const caseClasses: Lesson = {
  id: "case-classes",
  title: "Case Classes",
  chapterId: "oop",
  content: `## Case Classes

Case classes are immutable data containers with built-in \`equals\`, \`hashCode\`, and \`toString\`:

\`\`\`scala
case class Point(x: Int, y: Int)

val p = Point(3, 4)
println(p.x)  // 3
println(p.y)  // 4
\`\`\`

### Methods on Case Classes

\`\`\`scala
case class Circle(radius: Double) {
  def area: Double = 3.14159 * radius * radius
  def circumference: Double = 2 * 3.14159 * radius
}

val c = Circle(5.0)
println(c.area)
\`\`\`

### Your Task

Define a case class \`Rectangle(width: Int, height: Int)\` with two methods:
- \`area: Int\` — returns \`width * height\`
- \`perimeter: Int\` — returns \`2 * (width + height)\``,

  starterCode: `case class Rectangle(width: Int, height: Int) {
  def area: Int = width * height
  def perimeter: Int = 2 * (width + height)
}

val r = Rectangle(5, 4)
println(r.area)
println(r.perimeter)
`,

  solution: `case class Rectangle(width: Int, height: Int) {
  def area: Int = width * height
  def perimeter: Int = 2 * (width + height)
}

val r = Rectangle(5, 4)
println(r.area)
println(r.perimeter)
`,

  tests: [
    {
      name: "Rectangle(5,4) area = 20",
      expected: "20\n",
      code: `{{FUNC}}
val r = Rectangle(5, 4)
println(r.area)
`,
    },
    {
      name: "Rectangle(5,4) perimeter = 18",
      expected: "18\n",
      code: `{{FUNC}}
val r = Rectangle(5, 4)
println(r.perimeter)
`,
    },
    {
      name: "Rectangle(3,7) area = 21",
      expected: "21\n",
      code: `{{FUNC}}
val r = Rectangle(3, 7)
println(r.area)
`,
    },
  ],
};
