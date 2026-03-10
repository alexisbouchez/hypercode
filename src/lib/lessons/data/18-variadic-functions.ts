import type { Lesson } from "../types";

export const variadicFunctions: Lesson = {
  id: "variadic-functions",
  title: "Variadic Functions",
  chapterId: "functions",
  content: `## Variadic Functions

A variadic function accepts a variable number of arguments. You declare one by placing \`...\` before the parameter type:

\`\`\`go
func sum(nums ...int) int {
    total := 0
    for _, n := range nums {
        total += n
    }
    return total
}
\`\`\`

Inside the function, \`nums\` is a regular \`[]int\` slice. You can call it with any number of arguments:

\`\`\`go
sum(1, 2, 3)    // 6
sum(10, 20)     // 30
sum()           // 0
\`\`\`

### Passing a Slice with \`...\`

If you already have a slice, you can spread it into a variadic call using \`...\`:

\`\`\`go
numbers := []int{1, 2, 3, 4}
total := sum(numbers...)
\`\`\`

This is the same \`...\` token, but used on the caller side. Without it, the compiler will reject the call --- you cannot pass a slice where individual arguments are expected.

### Common Variadic Functions

You have already been using variadic functions. \`fmt.Println\` accepts \`...any\`:

\`\`\`go
fmt.Println("hello", 42, true) // hello 42 true
\`\`\`

The built-in \`append\` is also variadic --- it appends one or more elements to a slice:

\`\`\`go
s := []int{1, 2}
s = append(s, 3, 4, 5)
\`\`\`

You can even append one slice to another:

\`\`\`go
a := []int{1, 2}
b := []int{3, 4}
a = append(a, b...)
\`\`\`

### Rules

- Only the **last** parameter can be variadic.
- A variadic function can be called with **zero** arguments for the variadic parameter.
- The variadic parameter is \`nil\` (not an empty slice) when no arguments are passed.

### Your Task

Write a function \`joinWithSep\` that takes a \`string\` separator followed by a variadic \`...string\` parameter. It should return a single string with all the strings joined by the separator.

For example: \`joinWithSep(", ", "a", "b", "c")\` returns \`"a, b, c"\`.

If no strings are passed, return an empty string.`,

  starterCode: `package main

import "fmt"

func joinWithSep(sep string, parts ...string) string {
\t// Your code here
\treturn ""
}

func main() {
\tfmt.Println(joinWithSep(", ", "Go", "is", "fun"))
\tfmt.Println(joinWithSep("-", "2024", "01", "15"))
}
`,

  solution: `package main

import "fmt"

func joinWithSep(sep string, parts ...string) string {
\tif len(parts) == 0 {
\t\treturn ""
\t}
\tresult := parts[0]
\tfor _, p := range parts[1:] {
\t\tresult += sep + p
\t}
\treturn result
}

func main() {
\tfmt.Println(joinWithSep(", ", "Go", "is", "fun"))
\tfmt.Println(joinWithSep("-", "2024", "01", "15"))
}
`,

  tests: [
    {
      name: "Join with comma separator",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(joinWithSep(", ", "Go", "is", "fun"))
}`,
      expected: "Go, is, fun\n",
    },
    {
      name: "Join with dash separator",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(joinWithSep("-", "2024", "01", "15"))
}`,
      expected: "2024-01-15\n",
    },
    {
      name: "Join with no strings returns empty",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tresult := joinWithSep(", ")
\tfmt.Printf("[%s]\\n", result)
}`,
      expected: "[]\n",
    },
    {
      name: "Join single string",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tfmt.Println(joinWithSep(" ", "hello"))
}`,
      expected: "hello\n",
    },
  ],
};
