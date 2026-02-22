import type { Lesson } from "../../types";

export const optionals: Lesson = {
  id: "optionals",
  title: "Optionals",
  chapterId: "collections",
  content: `## Optionals

An optional represents a value that may or may not exist. You declare it by appending \`?\` to the type:

\`\`\`swift
var name: String? = "Alice"
name = nil  // now it has no value
\`\`\`

### Nil-Coalescing Operator

Use \`??\` to provide a default value when an optional is \`nil\`:

\`\`\`swift
let displayName = name ?? "Anonymous"
\`\`\`

### Optional Binding

Use \`if let\` to safely unwrap an optional:

\`\`\`swift
if let unwrapped = name {
    print("Hello, \\(unwrapped)")
} else {
    print("No name")
}
\`\`\`

### Returning Optionals

Functions can return optionals to signal possible failure:

\`\`\`swift
func findFirst(_ nums: [Int], _ target: Int) -> Int? {
    for (i, n) in nums.enumerated() {
        if n == target { return i }
    }
    return nil
}
\`\`\`

### Your Task

Write a function \`safeDivide\` that takes two integers \`a\` and \`b\`, and returns \`a / b\` as an \`Int?\`. Return \`nil\` if \`b\` is zero.`,

  starterCode: `func safeDivide(_ a: Int, _ b: Int) -> Int? {
    if b == 0 {
        return nil
    }
    return a / b
}

let result = safeDivide(10, 2) ?? -1
print(result)

let bad = safeDivide(10, 0) ?? -1
print(bad)
`,

  solution: `func safeDivide(_ a: Int, _ b: Int) -> Int? {
    if b == 0 {
        return nil
    }
    return a / b
}

let result = safeDivide(10, 2) ?? -1
print(result)

let bad = safeDivide(10, 0) ?? -1
print(bad)
`,

  tests: [
    {
      name: "10 / 2 = 5",
      expected: "5\n",
      code: `{{FUNC}}
let r = safeDivide(10, 2) ?? -1
print(r)
`,
    },
    {
      name: "division by zero returns nil",
      expected: "-1\n",
      code: `{{FUNC}}
let r = safeDivide(10, 0) ?? -1
print(r)
`,
    },
  ],
};
