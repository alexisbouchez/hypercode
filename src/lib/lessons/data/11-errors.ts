import type { Lesson } from "../types";

export const errors: Lesson = {
  id: "errors",
  title: "Error Handling",
  chapterId: "error-handling",
  content: `## Errors as Values

Go does not have exceptions. Instead, functions that can fail return an error value alongside their result. This is arguably Go's most important design decision.

### The error Interface

The built-in \`error\` type is an interface with a single method:

\`\`\`go
type error interface {
    Error() string
}
\`\`\`

Any type that has an \`Error() string\` method is an error. This is the simplest possible contract.

### Creating Errors

The standard library provides two ways to create simple errors:

\`\`\`go
import "errors"

err := errors.New("something went wrong")
\`\`\`

\`\`\`go
import "fmt"

err := fmt.Errorf("user %s not found", username)
\`\`\`

\`fmt.Errorf\` works like \`fmt.Sprintf\` but returns an error. Use it when you need formatted messages.

### The Error-Checking Pattern

The canonical Go pattern: call a function, check the error immediately, handle it or return it:

\`\`\`go
result, err := doSomething()
if err != nil {
    return fmt.Errorf("doSomething failed: %w", err)
}
// use result
\`\`\`

The \`%w\` verb wraps the original error, preserving the chain for debugging. This pattern appears hundreds of times in any real Go codebase.

### Custom Error Types

For richer error information, define your own error type:

\`\`\`go
type ValidationError struct {
    Field   string
    Message string
}

func (e *ValidationError) Error() string {
    return fmt.Sprintf("%s: %s", e.Field, e.Message)
}
\`\`\`

### When to Return Errors

A function should return an error when:
- An operation can fail (file I/O, network, parsing)
- The failure is recoverable (the caller can do something about it)
- The failure is expected in normal operation

Do not return errors for programming mistakes (like passing a nil pointer where one is never expected). Use panics for those.

### Your Task

Write a function \`validateAge\` that takes an \`int\` and returns an \`error\`:

- If age is negative, return an error with the message \`"age cannot be negative"\`
- If age is greater than 150, return an error with the message \`"age is unrealistic"\`
- Otherwise, return \`nil\` (no error)`,

  starterCode: `package main

import (
\t"errors"
\t"fmt"
)

func validateAge(age int) error {
\t// Your code here
\treturn nil
}

func main() {
\tfor _, age := range []int{25, -1, 200} {
\t\tif err := validateAge(age); err != nil {
\t\t\tfmt.Println(err)
\t\t} else {
\t\t\tfmt.Println("valid")
\t\t}
\t}
}
`,

  solution: `package main

import (
\t"errors"
\t"fmt"
)

func validateAge(age int) error {
\tif age < 0 {
\t\treturn errors.New("age cannot be negative")
\t}
\tif age > 150 {
\t\treturn errors.New("age is unrealistic")
\t}
\treturn nil
}

func main() {
\tfor _, age := range []int{25, -1, 200} {
\t\tif err := validateAge(age); err != nil {
\t\t\tfmt.Println(err)
\t\t} else {
\t\t\tfmt.Println("valid")
\t\t}
\t}
}
`,

  tests: [
    {
      name: "valid age (25)",
      code: `package main

import (
\t"errors"
\t"fmt"
)

{{FUNC}}

func main() {
\t_ = errors.New
\tif err := validateAge(25); err != nil {
\t\tfmt.Println(err)
\t} else {
\t\tfmt.Println("valid")
\t}
}`,
      expected: "valid\n",
    },
    {
      name: "negative age (-1)",
      code: `package main

import (
\t"errors"
\t"fmt"
)

{{FUNC}}

func main() {
\t_ = errors.New
\tif err := validateAge(-1); err != nil {
\t\tfmt.Println(err)
\t} else {
\t\tfmt.Println("valid")
\t}
}`,
      expected: "age cannot be negative\n",
    },
    {
      name: "unrealistic age (200)",
      code: `package main

import (
\t"errors"
\t"fmt"
)

{{FUNC}}

func main() {
\t_ = errors.New
\tif err := validateAge(200); err != nil {
\t\tfmt.Println(err)
\t} else {
\t\tfmt.Println("valid")
\t}
}`,
      expected: "age is unrealistic\n",
    },
    {
      name: "edge case: age 0 is valid",
      code: `package main

import (
\t"errors"
\t"fmt"
)

{{FUNC}}

func main() {
\t_ = errors.New
\tif err := validateAge(0); err != nil {
\t\tfmt.Println(err)
\t} else {
\t\tfmt.Println("valid")
\t}
}`,
      expected: "valid\n",
    },
  ],
};
