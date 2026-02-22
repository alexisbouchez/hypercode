import type { Lesson } from "../../types";

export const structs: Lesson = {
  id: "structs",
  title: "Structs",
  chapterId: "types",
  content: `## Structs

Structs are value types that group related data and behavior:

\`\`\`swift
struct Point {
    var x: Int
    var y: Int

    func distanceFromOrigin() -> Double {
        return Double(x * x + y * y)
    }
}

let p = Point(x: 3, y: 4)
print(p.x)  // 3
\`\`\`

### Memberwise Initializer

Swift automatically generates an initializer from the properties:

\`\`\`swift
let p = Point(x: 3, y: 4)  // free memberwise init
\`\`\`

### Mutating Methods

Structs are value types, so methods that modify properties must be marked \`mutating\`:

\`\`\`swift
struct Counter {
    var count = 0
    mutating func increment() { count += 1 }
}
\`\`\`

### Your Task

Create a \`Rectangle\` struct with \`width\` and \`height\` (both \`Int\`), and two methods:
- \`area() -> Int\` — returns \`width * height\`
- \`perimeter() -> Int\` — returns \`2 * (width + height)\``,

  starterCode: `struct Rectangle {
    var width: Int
    var height: Int

    func area() -> Int {
        return width * height
    }

    func perimeter() -> Int {
        return 2 * (width + height)
    }
}

let rect = Rectangle(width: 5, height: 4)
print(rect.area())
print(rect.perimeter())
`,

  solution: `struct Rectangle {
    var width: Int
    var height: Int

    func area() -> Int {
        return width * height
    }

    func perimeter() -> Int {
        return 2 * (width + height)
    }
}

let rect = Rectangle(width: 5, height: 4)
print(rect.area())
print(rect.perimeter())
`,

  tests: [
    {
      name: "area of 5x4",
      expected: "20\n",
      code: `{{FUNC}}
let r = Rectangle(width: 5, height: 4)
print(r.area())
`,
    },
    {
      name: "perimeter of 5x4",
      expected: "18\n",
      code: `{{FUNC}}
let r = Rectangle(width: 5, height: 4)
print(r.perimeter())
`,
    },
    {
      name: "area of 3x7",
      expected: "21\n",
      code: `{{FUNC}}
let r = Rectangle(width: 3, height: 7)
print(r.area())
`,
    },
  ],
};
