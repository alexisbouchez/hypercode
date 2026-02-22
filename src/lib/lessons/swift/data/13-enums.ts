import type { Lesson } from "../../types";

export const enums: Lesson = {
  id: "enums",
  title: "Enums",
  chapterId: "types",
  content: `## Enums

Enums define a type with a fixed set of cases:

\`\`\`swift
enum Direction {
    case north
    case south
    case east
    case west
}

let dir = Direction.north
\`\`\`

### Raw Values

Enums can have raw values of any type:

\`\`\`swift
enum Planet: String {
    case mercury = "Mercury"
    case venus   = "Venus"
    case earth   = "Earth"
}

print(Planet.earth.rawValue)  // Earth
\`\`\`

### Switching on Enums

\`\`\`swift
switch dir {
case .north:
    print("Going north")
case .south:
    print("Going south")
default:
    print("Going elsewhere")
}
\`\`\`

### Your Task

Define an enum \`Planet\` with \`String\` raw values for \`mercury\`, \`venus\`, \`earth\`, \`mars\`. Then write a function \`describe\` that takes a \`Planet\` and returns a string like \`"Earth is a planet."\`.`,

  starterCode: `enum Planet: String {
    case mercury = "Mercury"
    case venus = "Venus"
    case earth = "Earth"
    case mars = "Mars"
}

func describe(_ p: Planet) -> String {
    return "\\(p.rawValue) is a planet."
}

print(describe(Planet.earth))
print(describe(Planet.mars))
`,

  solution: `enum Planet: String {
    case mercury = "Mercury"
    case venus = "Venus"
    case earth = "Earth"
    case mars = "Mars"
}

func describe(_ p: Planet) -> String {
    return "\\(p.rawValue) is a planet."
}

print(describe(Planet.earth))
print(describe(Planet.mars))
`,

  tests: [
    {
      name: "describe Earth",
      expected: "Earth is a planet.\n",
      code: `{{FUNC}}
print(describe(Planet.earth))
`,
    },
    {
      name: "describe Mars",
      expected: "Mars is a planet.\n",
      code: `{{FUNC}}
print(describe(Planet.mars))
`,
    },
    {
      name: "describe Mercury",
      expected: "Mercury is a planet.\n",
      code: `{{FUNC}}
print(describe(Planet.mercury))
`,
    },
  ],
};
