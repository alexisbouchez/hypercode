import type { Lesson } from "../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions in Go

Functions are declared with \`func\`, followed by the name, parameters, and return type:

\`\`\`go
func greet(name string) string {
    return "Hello, " + name
}
\`\`\`

Parameter types come *after* the name, not before. This was a deliberate choice --- the Go team believes it reads more naturally, especially as declarations get complex.

When consecutive parameters share a type, you can group them:

\`\`\`go
func add(a, b int) int {
    return a + b
}
\`\`\`

### Multiple Return Values

This is one of Go's most distinctive features. A function can return more than one value:

\`\`\`go
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, fmt.Errorf("division by zero")
    }
    return a / b, nil
}
\`\`\`

The caller unpacks the results:

\`\`\`go
result, err := divide(10, 3)
\`\`\`

This pattern is the foundation of Go's error handling. Instead of exceptions, functions return errors as values.

### Named Return Values

You can name your return values, which documents what the function returns and allows "naked" returns:

\`\`\`go
func swap(a, b string) (first, second string) {
    first = b
    second = a
    return // returns first and second
}
\`\`\`

Use named returns sparingly. They are most useful for documenting return values in short functions. In longer functions, explicit returns are clearer.

### Functions as Values

Functions are first-class values. You can assign them to variables, pass them as arguments, and return them from other functions:

\`\`\`go
double := func(x int) int {
    return x * 2
}
fmt.Println(double(5)) // 10
\`\`\`

### Your Task

Write a function \`safeDivide\` that takes two \`float64\` parameters and returns two values: a \`float64\` result and a \`string\` error message.

- If the second argument is 0, return \`0\` and \`"division by zero"\`
- Otherwise, return the result of the division and an empty string \`""\``,

  starterCode: `package main

import "fmt"

func safeDivide(a, b float64) (float64, string) {
\t// Your code here
\treturn 0, ""
}

func main() {
\tresult, err := safeDivide(10, 3)
\tfmt.Printf("%.2f, %s\\n", result, err)

\tresult, err = safeDivide(10, 0)
\tfmt.Printf("%.2f, %s\\n", result, err)
}
`,

  solution: `package main

import "fmt"

func safeDivide(a, b float64) (float64, string) {
\tif b == 0 {
\t\treturn 0, "division by zero"
\t}
\treturn a / b, ""
}

func main() {
\tresult, err := safeDivide(10, 3)
\tfmt.Printf("%.2f, %s\\n", result, err)

\tresult, err = safeDivide(10, 0)
\tfmt.Printf("%.2f, %s\\n", result, err)
}
`,

  tests: [
    {
      name: "10 / 3 returns correct result",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tresult, errMsg := safeDivide(10, 3)
\tfmt.Printf("%.2f, %s\\n", result, errMsg)
}`,
      expected: "3.33, \n",
    },
    {
      name: "division by zero returns error",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tresult, errMsg := safeDivide(10, 0)
\tfmt.Printf("%.2f, %s\\n", result, errMsg)
}`,
      expected: "0.00, division by zero\n",
    },
    {
      name: "20 / 4 returns 5.00",
      code: `package main

import "fmt"

{{FUNC}}

func main() {
\tresult, errMsg := safeDivide(20, 4)
\tfmt.Printf("%.2f, %s\\n", result, errMsg)
}`,
      expected: "5.00, \n",
    },
  ],
};
