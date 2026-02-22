import type { Lesson } from "../../types";

export const classes: Lesson = {
  id: "classes",
  title: "Classes",
  chapterId: "types",
  content: `## Classes

Classes are reference types (unlike structs which are value types). Multiple variables can refer to the same instance:

\`\`\`swift
class Animal {
    var name: String

    init(_ name: String) {
        self.name = name
    }

    func speak() -> String {
        return "..."
    }
}

let a = Animal("Dog")
print(a.speak())  // ...
\`\`\`

### Inheritance

\`\`\`swift
class Dog: Animal {
    override func speak() -> String {
        return "Woof!"
    }
}

let d = Dog("Rex")
print(d.speak())  // Woof!
\`\`\`

### Your Task

Create a \`Counter\` class with:
- A stored property \`count\` initialized to 0
- An \`increment()\` method that adds 1 to \`count\`
- A \`value()\` method that returns the current \`count\``,

  starterCode: `class Counter {
    var count = 0

    func increment() {
        count += 1
    }

    func value() -> Int {
        return count
    }
}

let c = Counter()
c.increment()
c.increment()
c.increment()
print(c.value())
`,

  solution: `class Counter {
    var count = 0

    func increment() {
        count += 1
    }

    func value() -> Int {
        return count
    }
}

let c = Counter()
c.increment()
c.increment()
c.increment()
print(c.value())
`,

  tests: [
    {
      name: "counter after 3 increments",
      expected: "3\n",
      code: `{{FUNC}}
let c = Counter()
c.increment()
c.increment()
c.increment()
print(c.value())
`,
    },
    {
      name: "counter starts at 0",
      expected: "0\n",
      code: `{{FUNC}}
let c = Counter()
print(c.value())
`,
    },
  ],
};
