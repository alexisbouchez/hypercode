import type { Lesson } from "../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## The Only Loop You Need

Go has exactly one loop keyword: \`for\`. It does the work of \`for\`, \`while\`, and \`do-while\` from other languages.

### Classic For Loop

The three-component form, similar to C or Java:

\`\`\`go
for i := 0; i < 5; i++ {
    fmt.Println(i)
}
\`\`\`

### While-Style Loop

Drop the init and post statements. You get a while loop:

\`\`\`go
n := 1
for n < 100 {
    n *= 2
}
\`\`\`

### Infinite Loop

> In the TNG episode "Cause and Effect," the Enterprise gets stuck in a time loop, repeating the same disaster over and over. At least they did not have to debug an off-by-one error.

Drop everything. Use \`break\` to exit:

\`\`\`go
for {
    line := readInput()
    if line == "quit" {
        break
    }
}
\`\`\`

### Continue

\`continue\` skips to the next iteration:

\`\`\`go
for i := 0; i < 10; i++ {
    if i%2 == 0 {
        continue
    }
    fmt.Println(i) // only odd numbers
}
\`\`\`

### For Range

The \`range\` keyword iterates over slices, arrays, strings, maps, and channels. It gives you both the index and the value:

\`\`\`go
names := []string{"Alice", "Bob", "Charlie"}
for i, name := range names {
    fmt.Printf("%d: %s\\n", i, name)
}
\`\`\`

Use \`_\` to discard the index when you do not need it:

\`\`\`go
for _, name := range names {
    fmt.Println(name)
}
\`\`\`

### Your Task

Write a function \`fizzBuzz\` that takes an integer \`n\` and prints the numbers from 1 to \`n\` (inclusive), one per line, with these substitutions:
- Print \`"FizzBuzz"\` if the number is divisible by both 3 and 5
- Print \`"Fizz"\` if the number is divisible by 3
- Print \`"Buzz"\` if the number is divisible by 5
- Print the number otherwise`,

  starterCode: `package main

import "fmt"

func fizzBuzz(n int) {
\t// Your code here
}

func main() {
\tfizzBuzz(15)
}
`,

  solution: `package main

import "fmt"

func fizzBuzz(n int) {
\tfor i := 1; i <= n; i++ {
\t\tswitch {
\t\tcase i%15 == 0:
\t\t\tfmt.Println("FizzBuzz")
\t\tcase i%3 == 0:
\t\t\tfmt.Println("Fizz")
\t\tcase i%5 == 0:
\t\t\tfmt.Println("Buzz")
\t\tdefault:
\t\t\tfmt.Println(i)
\t\t}
\t}
}

func main() {
\tfizzBuzz(15)
}
`,

  tests: [
    {
      name: "fizzBuzz(15)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfizzBuzz(15)
}`,
      expected:
        "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n",
    },
    {
      name: "fizzBuzz(5)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfizzBuzz(5)
}`,
      expected: "1\n2\nFizz\n4\nBuzz\n",
    },
    {
      name: "fizzBuzz(1)",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfizzBuzz(1)
}`,
      expected: "1\n",
    },
  ],
};
