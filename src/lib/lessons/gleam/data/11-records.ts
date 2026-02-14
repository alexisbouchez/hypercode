import type { Lesson } from "../../types";

export const records: Lesson = {
  id: "records",
  title: "Records",
  chapterId: "custom-types",
  content: `## Records

Records in Gleam are custom types with a single variant that has labeled fields:

\`\`\`gleam
pub type Person {
  Person(name: String, age: Int)
}
\`\`\`

The type name and constructor name are the same. You create a record by calling the constructor:

\`\`\`gleam
let alice = Person(name: "Alice", age: 30)
\`\`\`

### Accessing Fields

You can access fields with dot notation:

\`\`\`gleam
alice.name  // "Alice"
alice.age   // 30
\`\`\`

### Updating Records

Since all data is immutable, you create new records with updated fields using the spread syntax:

\`\`\`gleam
let older_alice = Person(..alice, age: alice.age + 1)
\`\`\`

The \`..alice\` copies all fields from \`alice\`, then \`age: alice.age + 1\` overrides the age.

### Pattern Matching on Records

You can destructure records in case expressions and let bindings:

\`\`\`gleam
let Person(name: name, age: age) = alice
// name = "Alice", age = 30

case person {
  Person(name: "Admin", ..) -> "admin user"
  Person(name: name, age: age) if age >= 18 -> name <> " is an adult"
  Person(name: name, ..) -> name <> " is a minor"
}
\`\`\`

### Records with Multiple Variants

A custom type can have multiple variants, each with their own fields:

\`\`\`gleam
pub type Animal {
  Dog(name: String, breed: String)
  Cat(name: String, indoor: Bool)
  Fish(name: String, species: String)
}
\`\`\`

### Your Task

Define a \`Rectangle\` record type with \`width\` and \`height\` fields (both \`Int\`). Write a function \`area\` that calculates the area, and a function \`describe\` that returns a string in the format \`"<width>x<height> (area: <area>)"\`. Print descriptions for two rectangles.`,

  starterCode: `import gleam/io
import gleam/int

pub type Rectangle {
\t// Define the Rectangle type
}

fn area(rect: Rectangle) -> Int {
\t// Calculate the area
\t0
}

fn describe(rect: Rectangle) -> String {
\t// Return "<width>x<height> (area: <area>)"
\t""
}

pub fn main() {
\tlet a = Rectangle(width: 5, height: 3)
\tlet b = Rectangle(width: 10, height: 7)
\tio.println(describe(a))
\tio.println(describe(b))
}
`,

  solution: `import gleam/io
import gleam/int

pub type Rectangle {
\tRectangle(width: Int, height: Int)
}

fn area(rect: Rectangle) -> Int {
\trect.width * rect.height
}

fn describe(rect: Rectangle) -> String {
\tint.to_string(rect.width)
\t<> "x"
\t<> int.to_string(rect.height)
\t<> " (area: "
\t<> int.to_string(area(rect))
\t<> ")"
}

pub fn main() {
\tlet a = Rectangle(width: 5, height: 3)
\tlet b = Rectangle(width: 10, height: 7)
\tio.println(describe(a))
\tio.println(describe(b))
}
`,

  tests: [
    {
      name: "describes rectangles with area",
      expected: "5x3 (area: 15)\n10x7 (area: 70)\n",
    },
  ],
};
