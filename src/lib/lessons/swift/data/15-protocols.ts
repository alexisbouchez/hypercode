import type { Lesson } from "../../types";

export const protocols: Lesson = {
  id: "protocols",
  title: "Protocols",
  chapterId: "types",
  content: `## Protocols

A protocol defines a blueprint of methods and properties that conforming types must implement — similar to interfaces in other languages:

\`\`\`swift
protocol Greetable {
    func greet() -> String
}

class Person: Greetable {
    var name: String
    init(_ name: String) { self.name = name }
    func greet() -> String { return "Hello, I'm \\(name)" }
}
\`\`\`

### Protocol as a Type

You can use a protocol as a type to write polymorphic code:

\`\`\`swift
func introduce(_ g: Greetable) {
    print(g.greet())
}
\`\`\`

### Structs Can Conform Too

\`\`\`swift
struct Robot: Greetable {
    func greet() -> String { return "Beep boop." }
}
\`\`\`

### Your Task

Define a \`Shape\` protocol with an \`area() -> Int\` method. Then create a \`Circle\` class that conforms to \`Shape\` and has an \`init(_ radius: Int)\`. The area should be \`radius * radius * 3\` (integer approximation of π×r²).`,

  starterCode: `protocol Shape {
    func area() -> Int
}

class Circle: Shape {
    var radius: Int

    init(_ radius: Int) {
        self.radius = radius
    }

    func area() -> Int {
        return radius * radius * 3
    }
}

let c = Circle(5)
print(c.area())
`,

  solution: `protocol Shape {
    func area() -> Int
}

class Circle: Shape {
    var radius: Int

    init(_ radius: Int) {
        self.radius = radius
    }

    func area() -> Int {
        return radius * radius * 3
    }
}

let c = Circle(5)
print(c.area())
`,

  tests: [
    {
      name: "circle radius 5 area",
      expected: "75\n",
      code: `{{FUNC}}
let c = Circle(5)
print(c.area())
`,
    },
    {
      name: "circle radius 3 area",
      expected: "27\n",
      code: `{{FUNC}}
let c = Circle(3)
print(c.area())
`,
    },
  ],
};
