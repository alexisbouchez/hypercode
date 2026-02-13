import type { Lesson } from "../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "control-flow",
  content: `## Making Decisions

### If / Else

Go's \`if\` statement looks like most languages, minus the parentheses around the condition:

\`\`\`go
if x > 10 {
    fmt.Println("big")
} else if x > 5 {
    fmt.Println("medium")
} else {
    fmt.Println("small")
}
\`\`\`

The braces are mandatory, even for single-line bodies. This eliminates an entire class of bugs that other languages suffer from.

### If with Init Statement

Go has a unique feature: you can run a short statement before the condition. The variable you declare is scoped to the \`if\` block:

\`\`\`go
if length := len(name); length > 10 {
    fmt.Println("long name")
} else {
    fmt.Println("short name")
}
// length is not accessible here
\`\`\`

This pattern keeps variables tightly scoped. You will see it everywhere in Go, especially with error handling.

> Spock would approve of Go's conditionals. Clean, logical, and no room for human error. "Fascinating."

### Switch

Go's \`switch\` is cleaner than most languages. Cases do not fall through by default, so no \`break\` statements are needed:

\`\`\`go
switch day {
case "Monday":
    fmt.Println("start of the week")
case "Friday":
    fmt.Println("almost weekend")
default:
    fmt.Println("regular day")
}
\`\`\`

### Switch Without a Condition

A \`switch\` with no value acts as a clean alternative to long \`if-else\` chains:

\`\`\`go
switch {
case temp <= 0:
    fmt.Println("freezing")
case temp <= 15:
    fmt.Println("cold")
case temp <= 30:
    fmt.Println("warm")
default:
    fmt.Println("hot")
}
\`\`\`

This reads naturally and scales better than nested \`if-else\` blocks.

### Your Task

Write a function \`classifyTemp\` that takes an integer temperature in Celsius and returns a string:
- \`"freezing"\` if temp <= 0
- \`"cold"\` if temp <= 15
- \`"warm"\` if temp <= 30
- \`"hot"\` if temp > 30`,

  starterCode: `package main

import "fmt"

func classifyTemp(temp int) string {
\t// Your code here
\treturn ""
}

func main() {
\tfmt.Println(classifyTemp(-5))
\tfmt.Println(classifyTemp(10))
\tfmt.Println(classifyTemp(25))
\tfmt.Println(classifyTemp(35))
}
`,

  solution: `package main

import "fmt"

func classifyTemp(temp int) string {
\tswitch {
\tcase temp <= 0:
\t\treturn "freezing"
\tcase temp <= 15:
\t\treturn "cold"
\tcase temp <= 30:
\t\treturn "warm"
\tdefault:
\t\treturn "hot"
\t}
}

func main() {
\tfmt.Println(classifyTemp(-5))
\tfmt.Println(classifyTemp(10))
\tfmt.Println(classifyTemp(25))
\tfmt.Println(classifyTemp(35))
}
`,

  tests: [
    {
      name: "freezing (-5)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(classifyTemp(-5))
}`,
      expected: "freezing\n",
    },
    {
      name: "cold (10)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(classifyTemp(10))
}`,
      expected: "cold\n",
    },
    {
      name: "warm (25)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(classifyTemp(25))
}`,
      expected: "warm\n",
    },
    {
      name: "hot (35)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(classifyTemp(35))
}`,
      expected: "hot\n",
    },
    {
      name: "edge case: exactly 0",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(classifyTemp(0))
}`,
      expected: "freezing\n",
    },
  ],
};
