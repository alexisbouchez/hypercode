import type { Lesson } from "../../types";

export const customTypes: Lesson = {
  id: "custom-types",
  title: "Custom Types",
  chapterId: "custom-types",
  content: `## Defining Custom Types

Custom types in Gleam are sum types (also called algebraic data types or tagged unions). They let you define a type that can be one of several variants:

\`\`\`gleam
pub type Season {
  Spring
  Summer
  Autumn
  Winter
}
\`\`\`

Each variant is a constructor. You create values by calling the constructor:

\`\`\`gleam
let current = Summer
\`\`\`

### Variants with Data

Variants can carry data:

\`\`\`gleam
pub type Shape {
  Circle(radius: Float)
  Rectangle(width: Float, height: Float)
  Triangle(base: Float, height: Float)
}
\`\`\`

You create them by passing arguments:

\`\`\`gleam
let s = Circle(radius: 5.0)
let r = Rectangle(width: 10.0, height: 20.0)
\`\`\`

### Matching on Custom Types

Use \`case\` to handle each variant:

\`\`\`gleam
fn describe(shape: Shape) -> String {
  case shape {
    Circle(radius: r) -> "circle with radius " <> float.to_string(r)
    Rectangle(width: w, height: h) -> "rectangle " <> float.to_string(w) <> "x" <> float.to_string(h)
    Triangle(..) -> "triangle"
  }
}
\`\`\`

The \`..\` pattern ignores all fields of a variant.

### The Power of Sum Types

Sum types make impossible states impossible. Consider representing a traffic light:

\`\`\`gleam
pub type Light {
  Red
  Yellow
  Green
}
\`\`\`

There is no way to create an invalid light color. The type system guarantees correctness.

### Your Task

Define a \`Direction\` type with four variants: \`North\`, \`South\`, \`East\`, \`West\`. Write a function \`opposite\` that returns the opposite direction. Print the opposite of each direction.`,

  starterCode: `import gleam/io

pub type Direction {
\t// Define the four directions
}

fn opposite(dir: Direction) -> Direction {
\t// Return the opposite direction
\tNorth
}

fn to_string(dir: Direction) -> String {
\tcase dir {
\t\tNorth -> "North"
\t\tSouth -> "South"
\t\tEast -> "East"
\t\tWest -> "West"
\t}
}

pub fn main() {
\tio.println(to_string(opposite(North)))
\tio.println(to_string(opposite(South)))
\tio.println(to_string(opposite(East)))
\tio.println(to_string(opposite(West)))
}
`,

  solution: `import gleam/io

pub type Direction {
\tNorth
\tSouth
\tEast
\tWest
}

fn opposite(dir: Direction) -> Direction {
\tcase dir {
\t\tNorth -> South
\t\tSouth -> North
\t\tEast -> West
\t\tWest -> East
\t}
}

fn to_string(dir: Direction) -> String {
\tcase dir {
\t\tNorth -> "North"
\t\tSouth -> "South"
\t\tEast -> "East"
\t\tWest -> "West"
\t}
}

pub fn main() {
\tio.println(to_string(opposite(North)))
\tio.println(to_string(opposite(South)))
\tio.println(to_string(opposite(East)))
\tio.println(to_string(opposite(West)))
}
`,

  tests: [
    {
      name: "returns opposite directions",
      expected: "South\nNorth\nWest\nEast\n",
    },
  ],
};
